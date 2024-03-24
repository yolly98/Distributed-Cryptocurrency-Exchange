export class Socket {
  constructor(host, port, callback, keepalive) {
    this.callback = callback
    const url = 'ws://' + host + ':' + port + '/websocket'
    this.webSocket = new WebSocket(url)
    this.webSocket.onmessage = this.onWebSocketMessage
    this.keepalive = keepalive
    setTimeout(() => this.sendKeepalive(), keepalive);
  }

  sendKeepalive = () => {
    if (!this.webSocket)
      return
    this.webSocket.send(JSON.stringify({opcode: 'keepalive'}))
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