from channels.generic.websocket import AsyncJsonWebsocketConsumer
from channels.db import database_sync_to_async
from django.db import transaction
from django.utils import timezone
from .models import Room, Message

class ChatConsumer(AsyncJsonWebsocketConsumer):

    async def connect(self):
        self.room_id = self.scope['url_route']['kwargs']['room_id']

        self.username = self.scope['session'].get('username')

        if not self.username or not await self.room_exists():
            await self.close()
            return

        self.room_group_name = f'chat_{self.room_id}'

        await self.channel_layer.group_add(
            self.room_group_name,
            self.channel_name
        )

        await self.accept()

    async def disconnect(self, close_code):
        if hasattr(self, 'room_group_name'):
            await self.channel_layer.group_discard(
                self.room_group_name,
                self.channel_name
            )

    async def receive_json(self, content):
        message = content.get('message', '')

        if not isinstance(message, str):
            return

        message = message.strip()

        if not message or len(message) > 1000:
            return

        saved_message = await self.save_message(message)

        await self.channel_layer.group_send(
            self.room_group_name,
            {
                'type': 'chat_message',
                'id': saved_message['id'],
                'message': message,
                'username': self.username,
                'timestamp': saved_message['timestamp']
            }
        )

    async def chat_message(self, event):
        await self.send_json({
            'id': event['id'],
            'message': event['message'],
            'username': event['username'],
            'timestamp': event['timestamp']
        })

    @database_sync_to_async
    def room_exists(self):
        return Room.objects.filter(
            id=self.room_id
        ).exists()

    @database_sync_to_async
    def save_message(self, content):
        with transaction.atomic():
            message = Message.objects.create(
                room_id=self.room_id,
                username=self.username,
                content=content
            )

            latest_ids = list(
                Message.objects.filter(
                    room_id=self.room_id
                ).order_by('-id').values_list(
                    'id', flat=True
                )[:20]
            )

            Message.objects.filter(
                room_id=self.room_id
            ).exclude(
                id__in=latest_ids
            ).delete()

        return {
            'id': message.id,
            'timestamp': timezone.localtime(
                message.timestamp
            ).strftime('%H:%M')
        }