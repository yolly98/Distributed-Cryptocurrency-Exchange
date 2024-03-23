import '../css/App.css';
import { Component } from 'react';

class App extends Component {

  constructor(props) {
      super(props)
      this.state = {
        balance: 2344,
        available_cryptos: 24,
        market_value: 14
      }
  }

  render(){
    return (
      <div id='App'>
        <h1>Frontend</h1>

        <div id='content'>
          
          <div id='login-container'>
            <input type='text' placeholder='User Name'></input>
            <input type='text' placeholder='Crypto Name'></input>
            <button>LOGIN</button>
          </div>

          <label id='balance'>Balance: {this.state.balance}</label>
          <label id='available-cryptos'>Cryptos: {this.state.available_cryptos}</label>

          <h2>Operations</h2>
          <label id='market-value'>Crypto Market Value: {this.state.market_value}</label>
          <div id='op-container'>
            <div className='op'>
              <h3>Buy</h3>
              <input type='number' placeholder='Amount (euro)'></input>
              <button>BUY</button>
            </div>
            <div className='op'>
              <h3>Sell</h3>
              <input type='number' placeholder='Amount (crypto)'></input>
              <button>SELL</button>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default App;
