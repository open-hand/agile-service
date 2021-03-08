import React, {
  createContext, useContext, useEffect, useMemo,
} from 'react';
import { stores } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { useDetailContainerContext } from '@/components/detail-container/context';
import store, { ReleaseDetailStore } from './store';
import { ReleaseDetailProps } from '..';

interface Context extends ReleaseDetailProps {
  prefixCls: string,
  disabled?: boolean,
  store: ReleaseDetailStore,
  topAnnouncementHeight: number,

}
const ReleaseDetailContext = createContext({} as Context);
const { HeaderStore } = stores;

export function useReleaseDetailContext() {
  return useContext(ReleaseDetailContext);
}

const Provider: React.FC<ReleaseDetailProps> = ({
  children, disableInitStore, programId, ...restProps
}) => {
  const prefixCls = 'c7n-agile-release-detail';
  const { eventsMap } = useDetailContainerContext();
  // const propsEvents = eventsMap.get('pi_aim');
  const propsEvents = {} as any;
  const updateDetail = useMemo(() => {
    if (propsEvents?.updateAfter) {
      // @ts-ignore
      return (data: any) => propsEvents.updateAfter!(data, 'detail-update');
    }
    return undefined;
  }, [propsEvents?.updateAfter]);

  // store.init({ events: { update: updateDetail }, programId, disabled: restProps.disabled });
  useEffect(() => () => {
    store.clear();
  },
  []);
  useEffect(() => {
    store.setDisabled(!!restProps.disabled);
  }, [restProps.disabled]);
  // const { piAimStore } = usePIAimStore();
  const values = {
    ...restProps,
    programId,
    disabled: restProps.disabled || store.disabled,
    prefixCls,
    events: propsEvents,
    store,
    topAnnouncementHeight: HeaderStore.announcementClosed ? 0 : 50,
  };
  return (
    <ReleaseDetailContext.Provider value={values}>
      {children}
    </ReleaseDetailContext.Provider>
  );
};
export default observer(Provider);
