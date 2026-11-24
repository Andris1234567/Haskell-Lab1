from django.shortcuts import render, redirect, get_object_or_404
from .models import Room

def index(request):
    error = ''

    if request.method == 'POST':
        room_name = request.POST.get('room_name', '').strip()
        username = request.POST.get('username', '').strip()

        if room_name and username:
            if len(room_name) <= 100 and len(username) <= 50:
                room, created = Room.objects.get_or_create(
                    name=room_name
                )

                request.session['username'] = username

                return redirect('room', room_id=room.id)

        error = 'Ievadiet derīgu vārdu un istabas nosaukumu!'

    rooms = Room.objects.all().order_by('-created_at')

    return render(request, 'chat/index.html', {
        'rooms': rooms,
        'error': error
    })

def room(request, room_id):
    current_room = get_object_or_404(Room, id=room_id)

    username = request.session.get('username')

    if not username:
        return redirect('index')

    messages = list(
        current_room.messages.order_by('-id')[:20]
    )

    messages.reverse()

    return render(request, 'chat/room.html', {
        'room': current_room,
        'username': username,
        'messages': messages
    })