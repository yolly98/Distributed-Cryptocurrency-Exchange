import React, {Component} from 'react'
import '../css/Login.css'

class Login extends Component {

  state = {

  }

  login = () => {
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

    this.props.login(user, password)
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