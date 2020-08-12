import React, { useEffect, useCallback, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { Page, Header, Content } from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';
import openSetDefaultStatus from '../components/set-default-status';
import openConfirmLeave from './components/confirm-leave';
import StatusCirculationTable from './components/status-circulation-table';
import { TabComponentProps } from '..';
import { useStatusCirculationContext } from './index';
import { useStateMachineContext } from '../context';
import IssueTypeTab from '../components/issue-type-tab';
import Save from './components/save';

const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const { store } = useStatusCirculationContext();
  const { selectedType, setSelectedType } = useStateMachineContext();
  // const selectedTypeRef = useRef<string>(selectedType);
  // selectedTypeRef.current = selectedType;
  const refresh = useCallback(() => {
    if (selectedType) {
      store.getStatusList(selectedType);
    }
  }, [selectedType, store]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  // useEffect(() => () => {
  //   if (store.hasAction) {
  //     openConfirmLeave({
  //       onOk: async () => {
  //         await store.batchUpdateStatusTransform(selectedTypeRef.current);
  //       },
  //     });
  //   }
  // }, []);
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
          onClick={() => {
            openSelectExistStatus({
              issueTypeId: selectedType,
              onSubmit: () => {
                refresh();
              },
            });
          }}
        >
          添加已有状态
        </Button>
        <Button
          icon="playlist_add"
          onClick={() => {
            openCreateStatus({
              selectedIssueType: [selectedType],
              onSubmit: () => {
                refresh();
              },
            });
          }}
        >
          创建新的状态
        </Button>
        <Button
          icon="settings"
          onClick={() => {
            openSetDefaultStatus({
              issueTypeId: selectedType,
              statusList: store.statusList,
              onSubmit: refresh,
            });
          }}
        >
          设置初始状态
        </Button>
      </Header>
      <Content style={{ display: 'flex', flexDirection: 'column', paddingBottom: 0 }}>
        <IssueTypeTab
          selectedType={selectedType}
          setSelectedType={(newType) => {
            if (store.hasAction) {
              openConfirmLeave({
                onOk: async () => {
                  await store.batchUpdateStatusTransform(selectedType);
                  setSelectedType(newType);
                },
              });
            } else {
              setSelectedType(newType);
            }
          }}
        />
        {tab}
        <div style={{ flex: 1, overflow: 'hidden' }}>
          <StatusCirculationTable />
        </div>
        <Save />
      </Content>
    </Page>
  );
};

export default observer(StatusCirculation);
