import React, {
  createContext, useContext, useMemo,
} from 'react';
import { injectIntl } from 'react-intl';
import { inject } from 'mobx-react';
import { User } from '@/common/types';
import { StatusProps, UserValueCode } from '@/routes/work-calendar/types';
import useStore, { StoreProps } from './useStore';

interface UserItemProps {
  name: string,
  value: UserValueCode,
}

interface ContextProps {
  prefixCls: string,
  intlPrefix: string,
  formatMessage(arg0: object, arg1?: object): string,
  AppState: { userInfo: User },
  mainStore: StoreProps,
  USER_OPTION: UserItemProps[],
  DEFAULT_USER: UserValueCode[],
}

const Store = createContext({} as ContextProps);

export function useWorkCalendarStore() {
  return useContext(Store);
}

export const StoreProvider = injectIntl(inject('AppState')((props: any) => {
  const {
    children,
    intl: { formatMessage },
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
    formatMessage,
    mainStore,
    USER_OPTION,
    DEFAULT_USER,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
