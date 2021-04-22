import React, {
  createContext, useCallback, useContext, useEffect, useMemo,
} from 'react';
import { stores } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { useDetailContainerContext } from '@/components/detail-container/context';
import { useDetail } from '@/components/detail-container';

import { DataSet } from 'choerodon-ui/pro/lib';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getProjectId, getOrganizationId } from '@/utils/common';
import store, { ReleaseDetailStore } from './store';
import { ReleaseDetailProps } from '..';
import ReleaseStoryTableDataSet from './ReleaseStoryTableDataSet';
import ReleaseBugTableDataSet from './ReleaseBugTableDataSet';

interface Context extends ReleaseDetailProps {
  prefixCls: string,
  disabled?: boolean,
  store: ReleaseDetailStore,
  storyTableDataSet: DataSet,
  bugTableDataSet: DataSet,
  isInProgram: boolean,
  detailProps: any,
  topAnnouncementHeight: number,

}
const ReleaseDetailContext = createContext({} as Context);
const { HeaderStore } = stores;

export function useReleaseDetailContext() {
  return useContext(ReleaseDetailContext);
}

const Provider: React.FC<ReleaseDetailProps> = ({
  children, onUpdate, ...restProps
}) => {
  const prefixCls = 'c7n-agile-publish-version-detail';
  const { isInProgram, loading } = useIsInProgram();
  const storyTableDataSet = useMemo(() => new DataSet(store.current?.id ? ReleaseStoryTableDataSet(store.current?.id!) : {}), [store.current?.id]);
  const bugTableDataSet = useMemo(() => new DataSet(store.current?.id ? ReleaseBugTableDataSet(store.current?.id!) : {}), [store.current?.id]);
  const handleRefresh = useCallback(() => {
    console.log('update.... handleRefresh');
    storyTableDataSet.query();
    bugTableDataSet.query();
  }, [bugTableDataSet, storyTableDataSet]);
  // const propsEvents = {} as any;
  // const updateDetail = useMemo(() => {
  //   if (propsEvents?.update) {
  //     // @ts-ignore
  //     return (data: any) => propsEvents.update!(data);
  //   }
  //   return undefined;
  // }, [propsEvents?.update]);
  const [detailProps] = useDetail();
  const updateDetail = useMemo(() => () => onUpdate && onUpdate(), []);
  const { open } = detailProps;
  const selectIssue = useMemo(() => (id: string) => open({
    path: 'issue',
    props: {
      issueId: id,
      projectId: getProjectId(),
      organizationId: getOrganizationId(),
    },
    events: {
      update: () => {
        handleRefresh();
      },
    },
  }), [handleRefresh, open]);
  useEffect(() => {
    store.init({ events: { update: updateDetail, selectIssue }, disabled: restProps.disabled });
  }, [restProps.disabled, selectIssue, updateDetail]);
  useEffect(() => {
    store.clear();
    store.select(restProps.id);
    return () => {
      store.clear();
    };
  }, []);
  useEffect(() => {
    store.setDisabled(!!restProps.disabled);
  }, [restProps.disabled]);
  // const { piAimStore } = usePIAimStore();
  const values = {
    ...restProps,
    detailProps,
    disabled: restProps.disabled || store.disabled,
    prefixCls,
    // events: propsEvents,
    store,
    storyTableDataSet,
    bugTableDataSet,
    isInProgram,
    topAnnouncementHeight: HeaderStore.announcementClosed ? 0 : 50,
  };
  return (
    <ReleaseDetailContext.Provider value={values}>
      {!loading && children}
    </ReleaseDetailContext.Provider>
  );
};
export default observer(Provider);
