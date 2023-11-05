import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { DefaultRootState } from 'react-redux';
import { createOwner, getOwner, searchOwners, updateOwner } from '../api';
import { logout } from '../Login/state';
import {
  Owner,
  Loadable,
  SearchResult,
  Keyed,
  SearchRequest,
  initMemo,
  WithMemo,
  FullOwner,
} from '../types';

export const getOwners = createAsyncThunk<
  SearchResult<Keyed<Owner>>,
  SearchRequest,
  { state: DefaultRootState }
>('owners/getOwners', (p: SearchRequest, thunkAPI) => {
  const req: SearchRequest = {
    ...thunkAPI.getState().owners.searchRequest,
    ...p,
  };

  return searchOwners(req);
});

export const createOwnerAction = createAsyncThunk(
  'createOwner',
  async (p: Owner, thunkAPI) => {
    const res = await createOwner(p);
    thunkAPI.dispatch(getOwners({}) as any);
    return res.data;
  }
);

export const selectOwnerAction = createAsyncThunk(
  'owners/selectOwner',
  (oKey: number) => getOwner(oKey)
);

export const updateOwnerAction = createAsyncThunk(
  'owner/updateOwner',
  updateOwner
);

const initialState = {
  createModalOpen: false,
  createState: 'NotRequested' as Loadable<Keyed<Owner>>,
  updateState: 'NotRequested' as Loadable<'Ok'>,
  searchRequest: { page: 0, itemsPerPage: 10 } as SearchRequest,
  owners: initMemo({ rows: [], total: 0 }) as WithMemo<
    SearchResult<Keyed<Owner>>
  >,
  current: 'NotRequested' as Loadable<FullOwner>,
};

const ownersSlice = createSlice({
  name: 'owner',
  initialState,
  reducers: {
    openNewOwnerModal: (s) => {
      s.createState = 'NotRequested';
      s.createModalOpen = true;
    },
    closeOwnerModal: (s) => {
      s.createModalOpen = false;
    },
  },
  extraReducers: (b) =>
    b
      .addCase(createOwnerAction.pending, (s) => {
        s.createState = 'Loading';
      })
      .addCase(createOwnerAction.fulfilled, (s, a) => {
        s.createState = a.payload;
        s.createModalOpen = false;
      })
      .addCase(createOwnerAction.rejected, (s) => {
        s.createState = 'NetworkError';
      })
      .addCase(getOwners.pending, (s, a) => {
        s.searchRequest = { ...s.searchRequest, ...a.meta.arg };
        s.owners.value = 'Loading';
      })
      .addCase(getOwners.fulfilled, (s, a) => {
        s.owners.value = a.payload;
        s.owners.memo = a.payload;
      })
      .addCase(getOwners.rejected, (s) => {
        s.owners.value = 'NetworkError';
      })
      .addCase(selectOwnerAction.pending, (s) => {
        s.current = 'Loading';
      })
      .addCase(selectOwnerAction.fulfilled, (s, a) => {
        s.current = a.payload;
      })
      .addCase(selectOwnerAction.rejected, (s) => {
        s.current = 'NetworkError';
      })
      .addCase(updateOwnerAction.pending, (s) => {
        s.updateState = 'Loading';
      })
      .addCase(updateOwnerAction.fulfilled, (s) => {
        s.updateState = 'Ok';
      })
      .addCase(updateOwnerAction.rejected, (s) => {
        s.updateState = 'NetworkError';
      })
      .addCase('logout/pending', () => initialState),
});

export const { openNewOwnerModal, closeOwnerModal } = ownersSlice.actions;

export default ownersSlice.reducer;
