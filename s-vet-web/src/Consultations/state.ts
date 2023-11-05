import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { push } from 'connected-react-router';
import { DefaultRootState } from 'react-redux';
import { matchPath } from 'react-router';
import {
  cancelConsultation,
  createConsultation,
  getConsultation,
  searchConsultations,
  updateConsultation,
} from '../api';
import { getHomeData } from '../Home/state';
import { logout } from '../Login/state';
import {
  Consultation,
  ConsultationSearchRequest,
  FullConsultation,
  FullWithPrevious,
  initMemo,
  isOk,
  Keyed,
  Loadable,
  SearchResult,
  WithMemo,
  WithPet,
} from '../types';

export const getConsultations = createAsyncThunk<
  SearchResult<FullConsultation>,
  ConsultationSearchRequest,
  { state: DefaultRootState }
>(
  'consultations/getConsultations',
  (p: ConsultationSearchRequest, thunkAPI) => {
    const req: ConsultationSearchRequest = {
      ...thunkAPI.getState().consultations.searchRequest,
      ...p,
    };

    return searchConsultations(req);
  }
);

export const createConsultationAction = createAsyncThunk<
  unknown,
  { startImmediately: boolean; consultation: WithPet<Consultation> },
  { state: DefaultRootState }
>('consultations/createConsultation', async (c, thunkAPI) => {
  const res = await createConsultation(c.consultation);
  const state = thunkAPI.getState();

  thunkAPI.dispatch(getHomeData());
  thunkAPI.dispatch(getConsultations(state.consultations.searchRequest));

  if (c.startImmediately) {
    thunkAPI.dispatch(push(`/consultations/${res.data.key}`));
  }

  return res.data;
});

export const updateConsultationAction = createAsyncThunk<
  Keyed<Consultation>,
  Keyed<Consultation>,
  { state: DefaultRootState }
>(
  'consultations/updateConsultation',
  async (c: Keyed<Consultation>, thunkAPI) => {
    const res = await updateConsultation(c);
    const state = thunkAPI.getState();
    const { current } = state.consultations;

    thunkAPI.dispatch(getHomeData());
    thunkAPI.dispatch(getConsultations(state.consultations.searchRequest));

    const m = matchPath<{ id: string }>(state.router.location.pathname, {
      path: '/consultations/:id',
      exact: true,
    });

    if (!!m && isOk(current) && current.key !== res.key) {
      thunkAPI.dispatch(selectConsultationAction(current.key));
    }

    return res;
  }
);

export const cancelConsultationAction = createAsyncThunk<
  void,
  number,
  { state: DefaultRootState }
>('consultations/cancelConsultation', async (cKey, thunkAPI) => {
  await cancelConsultation(cKey);
  const state = thunkAPI.getState();
  const { current } = state.consultations;

  const m = matchPath<{ id: string }>(state.router.location.pathname, {
    path: '/consultations/:id',
    exact: true,
  });

  if (!!m && isOk(current)) {
    thunkAPI.dispatch(selectConsultationAction(current.key));
  }

  switch (state.router.location.pathname) {
    case '/':
      thunkAPI.dispatch(getHomeData());
      break;
    case '/consultations':
      thunkAPI.dispatch(getConsultations(state.consultations.searchRequest));
      break;
  }
});

export const selectConsultationAction = createAsyncThunk(
  'consultations/selectConsultation',
  (cKey: number) => getConsultation(cKey)
);

const initialState = {
  createModalOpen: false,
  createState: 'NotRequested' as Loadable<'Ok'>,
  cancelState: 'NotRequested' as Loadable<'Ok'>,
  updateState: 'NotRequested' as Loadable<'Ok'>,
  searchRequest: {
    page: 0,
    itemsPerPage: 10,
  } as ConsultationSearchRequest,
  consultations: initMemo({ rows: [], total: 0 }) as WithMemo<
    SearchResult<FullConsultation>
  >,
  current: 'NotRequested' as Loadable<FullWithPrevious>,
};

const consultationsSlice = createSlice({
  name: 'consultations',
  initialState: {
    createModalOpen: false,
    createState: 'NotRequested' as Loadable<'Ok'>,
    cancelState: 'NotRequested' as Loadable<'Ok'>,
    updateState: 'NotRequested' as Loadable<'Ok'>,
    searchRequest: {
      page: 0,
      itemsPerPage: 10,
    } as ConsultationSearchRequest,
    consultations: initMemo({ rows: [], total: 0 }) as WithMemo<
      SearchResult<FullConsultation>
    >,
    current: 'NotRequested' as Loadable<FullWithPrevious>,
  },
  reducers: {
    openNewConsultationModal: (s) => {
      s.createState = 'NotRequested';
      s.createModalOpen = true;
    },
    closeConsultationModal: (s) => {
      s.createModalOpen = false;
    },
  },
  extraReducers: (b) =>
    b
      .addCase(createConsultationAction.pending, (s) => {
        s.createState = 'Loading';
      })
      .addCase(createConsultationAction.fulfilled, (s) => {
        s.createState = 'Ok';
        s.createModalOpen = false;
      })
      .addCase(createConsultationAction.rejected, (s) => {
        s.createState = 'NetworkError';
      })
      .addCase(cancelConsultationAction.pending, (s) => {
        s.cancelState = 'Loading';
      })
      .addCase(cancelConsultationAction.fulfilled, (s) => {
        s.cancelState = 'Ok';
      })
      .addCase(cancelConsultationAction.rejected, (s) => {
        s.cancelState = 'NetworkError';
      })
      .addCase(updateConsultationAction.pending, (s) => {
        s.updateState = 'Loading';
      })
      .addCase(updateConsultationAction.fulfilled, (s) => {
        s.updateState = 'Ok';
      })
      .addCase(updateConsultationAction.rejected, (s) => {
        s.updateState = 'NetworkError';
      })
      .addCase(getConsultations.pending, (s, a) => {
        s.searchRequest = { ...s.searchRequest, ...a.meta.arg };
        s.consultations.value = 'Loading';
      })
      .addCase(getConsultations.fulfilled, (s, a) => {
        s.consultations.value = a.payload;
        s.consultations.memo = a.payload;
      })
      .addCase(getConsultations.rejected, (s) => {
        s.consultations.value = 'NetworkError';
      })
      .addCase(selectConsultationAction.pending, (s) => {
        s.current = 'Loading';
      })
      .addCase(selectConsultationAction.fulfilled, (s, a) => {
        s.current = a.payload;
      })
      .addCase(selectConsultationAction.rejected, (s) => {
        s.current = 'NetworkError';
      })
      .addCase('logout/pending', () => initialState),
});

export const {
  openNewConsultationModal,
  closeConsultationModal,
} = consultationsSlice.actions;

export default consultationsSlice.reducer;
