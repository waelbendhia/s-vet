import { configureStore, getDefaultMiddleware } from '@reduxjs/toolkit';
import { createBrowserHistory } from 'history';
import { routerMiddleware, connectRouter } from 'connected-react-router';
import { combineReducers, Store } from 'redux';
import * as Login from '../Login';
import * as Home from '../Home';
import * as Consultations from '../Consultations';
import * as Pets from '../Pets';
import * as Owners from '../Owners';
import * as Acts from '../Acts';
import * as Settings from '../Settings';
import * as Dashboard from '../Dashboard';

export const history = createBrowserHistory({
  basename: process.env.PUBLIC_URL,
});

const store = configureStore({
  middleware: [...getDefaultMiddleware(), routerMiddleware(history)],
  reducer: combineReducers({
    login: Login.reducer,
    home: Home.reducer,
    consultations: Consultations.reducer,
    pets: Pets.reducer,
    owners: Owners.reducer,
    acts: Acts.reducer,
    settings: Settings.reducer,
    dashboard: Dashboard.reducer,
    router: connectRouter(history),
  }),
});

export type State = typeof store extends Store<infer S, any> ? S : never;

declare module 'react-redux' {
  interface DefaultRootState {
    login: ReturnType<typeof Login.reducer>;
    home: ReturnType<typeof Home.reducer>;
    consultations: ReturnType<typeof Consultations.reducer>;
    pets: ReturnType<typeof Pets.reducer>;
    owners: ReturnType<typeof Owners.reducer>;
    acts: ReturnType<typeof Acts.reducer>;
    settings: ReturnType<typeof Settings.reducer>;
    dashboard: ReturnType<typeof Dashboard.reducer>;
    router: ReturnType<ReturnType<typeof connectRouter>>;
  }
}

export default store;
