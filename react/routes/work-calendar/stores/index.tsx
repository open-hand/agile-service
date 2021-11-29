import React, {
  createContext, useContext, useMemo,
} from 'react';
import { inject } from 'mobx-react';
import { User } from '@/common/types';
import { StatusProps, UserValueCode } from '@/routes/work-calendar/types';
import useStore, { StoreProps } from './useStore';

export const STATUS_COLOR: StatusProps = {
  todo: '#FFE9B6',
  prepare: '#FFE9B6',
  doing: '#CFE1FF',
  done: '#d8f0ec',
};

export const STATUS: StatusProps = {
  todo: '#F7C552',
  prepare: '#F7C552',
  doing: '#74A9FF',
  done: '#2FD2BC',
};

interface UserItemProps {
  name: string,
  value: UserValueCode,
}

interface ContextProps {
  prefixCls: string,
  intlPrefix: string,
  AppState: { userInfo: User },
  mainStore: StoreProps,
  USER_OPTION: UserItemProps[],
  DEFAULT_USER: UserValueCode[],
}

const Store = createContext({} as ContextProps);

export function useWorkCalendarStore() {
  return useContext(Store);
}

export const StoreProvider = inject('AppState')((props: any) => {
  const {
    children,
  } = props;
  const intlPrefix = 'c7nag.work.calendar';

  const USER_OPTION = useMemo(() => ([{
    name: '我经办的',
    value: 'assignee',
  }, {
    name: '我参与的',
    value: 'participant',
  }]), []);
  const DEFAULT_USER: UserValueCode[] = useMemo(() => ['assignee', 'participant'], []);
  const mainStore = useStore({ DEFAULT_USER });

  const value = {
    ...props,
    intlPrefix,
    prefixCls: 'c7nag-work-calendar',
    mainStore,
    USER_OPTION,
    DEFAULT_USER,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
});
