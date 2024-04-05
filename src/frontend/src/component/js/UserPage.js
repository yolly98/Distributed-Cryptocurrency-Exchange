import React, {Component} from 'react'
import '../css/UserPage.css'

class UserPage extends Component {

  state = {
    user: null
  }

  componentDidMount() {
    let user = sessionStorage.getItem('user')
    if (!user)
      window.location.href = '/login'
    this.setState({user})
  }

  render() {
    return(
      <h1>USER PAGE {this.state.user}</h1>
    )
  }
}

export default UserPage