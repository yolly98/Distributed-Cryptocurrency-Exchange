export class Socket {
  constructor(host, port, callback, keepalive) {
    this.callback = callback
    this.url = 'ws://' + host + ':' + port + '/websocket'
    this.webSocket = new WebSocket(this.url)
    this.webSocket.onmessage = this.onWebSocketMessage
    this.keepalive = keepalive
    setTimeout(() => this.sendKeepalive(), keepalive);
  }

  sendKeepalive = () => {
    if (!this.webSocket)
      return
    switch (this.webSocket.readyState) {
      case WebSocket.CLOSED:
        this.webSocket = new WebSocket(this.url)
        break
      case WebSocket.OPEN:
        this.webSocket.send(JSON.stringify({opcode: 'keepalive'}))
        break
    }
    setTimeout(() => this.sendKeepalive(), this.keepalive);
  }

  onWebSocketMessage = (event) => {
    let message = JSON.parse(event.data)
    this.callback(message)
  }

  close = () => {
    this.webSocket.close()
    this.webSocket = null
    this.callback = null
  }
}