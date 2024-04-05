import React, {Component} from 'react'
import '../css/Login.css'

class Login extends Component {

  state = {
    host: null,
    port: null
  }

  componentDidMount() {
    // load configuration
    fetch('/config.json')
    .then(response => response.json())
    .then(config => {
      this.setState({
        host: config.host,
        port: config.port
      })
    })
    .catch(error => console.error(error));
  }

  login = async () => {
    let user = document.getElementById('user-input').value
    let password = document.getElementById('password-input').value

    if (!user || user == '') {
      alert('Invalid Username')
      return
    }
    if (!password || password == '') {
      alert('Invalid Password')
      return
    }

    let url = 'http://' + this.state.host + ':' + this.state.port + '/api/authentication'
    let request = {
      type: 'login',
      user: user,
      password: password
    }
    let response = await fetch(url, {
      method : 'POST',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify(request)
    })

    if (response.status != 200) {
      alert('Login failed')
      return
    }

    this.props.login(user, password)
  }

  signup = async () => {
    let user = document.getElementById('user-input').value
    let password = document.getElementById('password-input').value

    if (!user || user == '') {
      alert('Invalid Username')
      return
    }
    if (!password || password == '') {
      alert('Invalid Password')
      return
    }

    let url = 'http://' + this.state.host + ':' + this.state.port + '/api/authentication'
    let request = {
      type: 'signup',
      user: user,
      password: password
    }
    let response = await fetch(url, {
      method : 'POST',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify(request)
    })

    if (response.status != 200) {
      alert('Signup failed')
      return
    } else {
      alert('Signup completed')
    }
  }

  render() {
    return(
      <div id='login'>
        <div id='login-container'>
          <h1>Distributed Cryptocurrency Exchange</h1>
          <input id='user-input' type='text' placeholder='User Name'></input>
          <input id='password-input' type='password' placeholder='Password'></input>
          <div id='login-button-container'>
            <button onClick={() => this.login()}>LOGIN</button>
            <button onClick={() => this.signup()}>SIGN UP</button>
          </div>
        </div>
      </div>
    )
  }
}

export default Login