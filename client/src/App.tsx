import { FC, useContext } from 'react';
import './App.scss';
import { Toaster } from 'react-hot-toast';
import WebSocketContext from './contexts/WebSocketContext';
import LoginView from './components/LoginView/LoginView';
import Inbox from './components/Inbox/Inbox';
import ChannelList from './components/ChannelList/ChannelList';

const App: FC = () => {
  const { connected } = useContext(WebSocketContext)

  return (
    <div className="App">
      {!connected && <LoginView />}

      {
        connected && (
          <>
            <ChannelList />
            <Inbox />
          </>
        )
      }

      <Toaster
        position='bottom-center'
      />
    </div>
  );
}

export default App;
