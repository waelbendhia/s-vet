import './App.less';
import { Button, Layout, Result, Spin } from 'antd';
import { Route, Switch } from 'react-router-dom';
import Login from './Login';
import { useDispatch, useSelector } from 'react-redux';
import React from 'react';
import { getAccount } from './Login/state';
import Home from './Home';
import Consultations, {
  Consultation,
  NewConsultationModal,
} from './Consultations';
import Pets, { NewPetModal, Pet } from './Pets';
import Owners, { NewOwnerModal, Owner } from './Owners';
import { ConnectedRouter } from 'connected-react-router';
import { history } from './redux/store';
import { NewActModal, UpdateActModal } from './Acts';
import Settings from './Settings';
import Dashboard from './Dashboard';
import Navigation from './Navigation';

const App = () => {
  const account = useSelector((s) => s.login.account);
  const dispatch = useDispatch();
  React.useEffect(() => {
    if (account === 'NotRequested') {
      dispatch(getAccount());
    }
  }, [account, dispatch]);

  return (
    <ConnectedRouter history={history}>
      <NewConsultationModal />
      <NewPetModal />
      <NewOwnerModal />
      <NewActModal />
      <UpdateActModal />
      <Layout>
        <Navigation />
        <Layout.Content>
          {typeof account === 'object' ? (
            <Switch>
              <Route
                path="/consultations/:key"
                exact
                component={Consultation}
              />
              <Route path="/consultations" exact component={Consultations} />
              <Route path="/owners/:key" exact component={Owner} />
              <Route path="/owners" exact component={Owners} />
              <Route path="/pets/:key" exact component={Pet} />
              <Route path="/pets" exact component={Pets} />
              <Route path="/dashboard" exact component={Dashboard} />
              <Route path="/settings" exact component={Settings} />
              <Route path="/" exact component={Home} />
            </Switch>
          ) : account === 'Outsider' ? (
            <Login />
          ) : account === 'NetworkError' ? (
            <Result
              title="Erreur"
              subTitle="Une erreur est survenue, veuillez contacter Wael"
              extra={
                <Button onClick={() => dispatch(getAccount())}>
                  Reessayer
                </Button>
              }
            />
          ) : (
            <Spin className="full-page-spin" spinning />
          )}
        </Layout.Content>
      </Layout>
    </ConnectedRouter>
  );
};

export default App;
