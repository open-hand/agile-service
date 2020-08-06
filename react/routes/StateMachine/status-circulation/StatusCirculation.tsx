import React, { useEffect } from 'react';
import { Page, Header, Content } from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';
import openSetDefaultStatus from '../components/set-default-status';
import StatusCirculationTable from './components/status-circulation-table';
import { TabComponentProps } from '..';
import { useStatusCirculationContext } from './index';

const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const { store } = useStatusCirculationContext();
  useEffect(() => {
    store.getStatusList();
  });
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
          onClick={() => {
            openSelectExistStatus({
              onSubmit: () => {

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
              onSubmit: () => {

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
              onSubmit: () => {

              },
            });
          }}
        >
          设置初始状态
        </Button>
      </Header>
      <Content>
        {tab}
        <StatusCirculationTable />
      </Content>
    </Page>
  );
};

export default StatusCirculation;
