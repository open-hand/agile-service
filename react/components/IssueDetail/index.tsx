import React, {
  useCallback, useEffect, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Animate } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import DetailContainer from '@/components/detail-container';
import Container from './Container';
import Store from './store';
import IssueDetailContext from './context';
import { useDetailContainerContext } from '../detail-container/context';

export type DemandEvents = 'update' | 'delete' | 'transfer' | 'close'
const { HeaderStore, AppState } = stores;
export type Events = { [key in DemandEvents]?: Function };
export function useDetailStore() {
  return useMemo(() => new Store(), []);
}
interface Props {
  store: Store
  projectId?: number,
  organizationId?: string,
  outside?: boolean
}

const IssueDetail: React.FC<Props> = ({
  projectId, organizationId, store, outside = false,
}) => {
  const { visible, selected } = store;
  console.log(visible);
  useEffect(() => {
    store.load(outside, organizationId);
  }, [organizationId, outside, selected, store]);
  // const { setVisible } = useDetailContainerContext();
  // useEffect(() => {
  //   setVisible(visible);
  // }, [setVisible, visible]);
  // 离开一个页面时，清空数据
  useEffect(() => {
    store.initApi(outside, organizationId);
    return () => {
      store.destroy();
    };
  }, [organizationId, outside, store]);
  // 编辑的限制
  const checkEnableEditDetail = useCallback((hasPermission: boolean) => false, []);

  return (
    <IssueDetailContext.Provider value={{
      id: selected,
      store,
      projectId,
      hasAdminPermission: false,
      disabledDetailEdit: !checkEnableEditDetail(false),
      outside: false,
      organizationId,
      topAnnouncementHeight: HeaderStore.announcementClosed ? 0 : 50,
      applyType: 'agile',
    }}
    >
      <Container />
    </IssueDetailContext.Provider>
  );
};
const ObserverIssueDetail = observer(IssueDetail);
export default ObserverIssueDetail;
// export default (props:Props) => (
//   <DetailContainer>
//     <ObserverIssueDetail {...props} />
//   </DetailContainer>
// );
