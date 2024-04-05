import React, {Component, Suspense} from 'react'
import { BrowserRouter as Router, Route, Routes, Link, useLocation} from 'react-router-dom'
import '../css/App.css'

const Trade = React.lazy(() => import('./Trade'))
const Login = React.lazy(() => import('./Login'))
const UserPage = React.lazy(() => import('./UserPage'))
const NotFound = React.lazy(() => import('./NotFound'))

class App extends Component {

  state = {
    user: null,
    config: null
  }

  componentDidMount() {
    fetch('/config.json')
    .then(response => response.json())
    .then(config => {
        this.setState({config})
    })
    .catch(error => console.error(error));
  }

  login = async (user, password) => {
  
    // TODO REST login request
    sessionStorage.setItem('user', user)
    this.setState({user}, () => {
      document.getElementById('link_to_user').click()
    })
  }

  render() {

    let link_to_trade = <></>
    if (this.state.config)
      link_to_trade = <Link id='link_to_user' to={`/trade/${this.state.config.default_coin}`} style={{pointerEvents: 'none', visibility: 'hidden'}}/>
    return(
      <>
        <Router>
          {link_to_trade}
          <Suspense>
            <Routes>
              <Route
                exact path = '/login'
                element = {<Login login={this.login}/>}
              />
              <Route
                exact path = ''
                element = {<Login login={this.login}/>}
              />
              <Route
                exact path = '/trade/:coinId'
                element = {<Trade />}
              />
              <Route
                exact path = '/user'
                element = {<UserPage />}
              />
              <Route path='*' element={<NotFound />} />
            </Routes>
          </Suspense>
      </Router>
      </>
    ) 
  }
}

export default App