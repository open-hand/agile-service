/* eslint-disable react/jsx-no-bind */
import React, {
  useState, useEffect, useContext, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Table } from 'choerodon-ui/pro';
import {
  Content, Header, TabPage as Page, Breadcrumb, useTheme, Action,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { getStageMap } from '@/utils/stateMachine';
import Store from './stores';
import openStateModal from './StateModal';
import openDeleteModal from './components/DeleteModal';
import { Loading } from '@/components';
import C7NTooltip from '@/components/c7n-tooltip';
import Style from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const backlogStates = ['backlog_pending_approval', 'backlog_rejected', 'backlog_create', 'backlog_planning', 'backlog_processing', 'backlog_developed', 'backlog_publish'];

const stageMap = getStageMap();
const { Column } = Table;

function StateList(props) {
  const [theme] = useTheme();
  const context = useContext(Store);
  const { stateStore, tableDs } = context;
  const formatMessage = useFormatMessage();
  const [initialTotal, setInitialTotal] = useState(0);

  const loadData = useCallback(async () => {
    await tableDs.query();
    setInitialTotal(tableDs.totalCount);
  }, []);

  useEffect(() => {
    loadData();
  }, []);

  const handleOnOk = useCallback(() => {
    tableDs.query(tableDs.currentPage);
  }, [tableDs.currentPage]);

  const confirmDelete = (record) => {
    openDeleteModal({ id: record.get('id'), name: record.get('name'), onOk: handleOnOk });
  };

  const renderName = useCallback(({ value, record }) => (
    <C7NTooltip
      className={Style.name}
      onClick={() => openStateModal({
        onOk: handleOnOk,
        statusId: record.get('id'),
        name: value,
        disabledEditName: backlogStates.includes(record.get('code')),
      })}
    >
      {value}
    </C7NTooltip>
  ), []);

  const renderAction = useCallback(({ record }) => {
    const actionData = [
      {
        text: formatMessage({ id: 'boot.delete' }),
        action: () => confirmDelete(record),
      },
    ];
    return !(record.get('code') || (record.get('stateMachineInfoList') && record.get('stateMachineInfoList')?.length)) ? <Action data={actionData} /> : null;
  }, [handleOnOk, confirmDelete, backlogStates]);

  const renderType = useCallback(({ value }) => (
    <div>
      <div className="issue-state-block" style={{ backgroundColor: stageMap[value]?.colour }} />
      <span style={{ verticalAlign: 'middle' }}>{stageMap[value]?.name}</span>
    </div>
  ), []);

  return (
    <Page>
      <Header>
        <HeaderButtons items={[
          {
            name: formatMessage({ id: 'agile.stateMachine.create.state' }),
            icon: 'playlist_add',
            display: true,
            disabled: !initialTotal,
            handler: () => { openStateModal({ onOk: handleOnOk }); },
            tooltipsConfig: {
              title: initialTotal ? undefined : '请创建项目后再创建状态机',
            },
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Content className="issue-state-content" style={theme === 'theme4' ? undefined : { paddingTop: 0 }}>
        <Loading loading={stateStore.getIsLoading} className={Style.tableWrap}>
          <Table dataSet={tableDs} className="issue-table">
            <Column name="name" renderer={renderName} />
            <Column renderer={renderAction} width={55} />
            <Column name="description" tooltip="overflow" />
            <Column name="type" renderer={renderType} />
          </Table>
        </Loading>
      </Content>
    </Page>
  );
}
// withRouter 原先在这包裹
export default observer(StateList);
