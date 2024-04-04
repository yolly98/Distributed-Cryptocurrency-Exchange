import React, {Component} from 'react'
import '../css/UserPage.css'

class UserPage extends Component {

  state = {

  }

  componentDidMount() {
    if (!this.props.user)
      window.location.href = '/login'
  }

  render() {
    return(
      <h1>USER PAGE {this.props.user}</h1>
    )
  }
}

export default UserPage