/* eslint-disable jsx-a11y/click-events-have-key-events */
import React, { useMemo, useCallback } from 'react';
import { axios, Choerodon } from '@choerodon/boot';
import {
  Table, DataSet, Menu, Dropdown, Modal,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { TableColumnTooltip, TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { User } from '@/common/types';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { getProjectId } from '@/utils/common';
import UserHead from '@/components/UserHead';
import { pageRuleApi } from '@/api';
import styles from '../../index.less';
import RuleModal from '../rule-modal';

const { Column } = Table;

interface Props{
    tableDataSet: DataSet,
}

const RuleTable: React.FC<Props> = ({ tableDataSet }) => {
  const handleClickName = useCallback((ruleId: string) => {
    Modal.open({
      className: styles.rule_modal,
      drawer: true,
      style: {
        width: 740,
      },
      key: Modal.key(),
      title: '编辑规则',
      children: <RuleModal ruleTableDataSet={tableDataSet} ruleId={ruleId} />,
    });
  }, [tableDataSet]);

  const renderName = useCallback(({ value, record }) => (
    <span
      role="none"
      className={styles.name}
      onClick={() => handleClickName(record?.get('id'))}
    >
      {value || '我是name'}
    </span>
  ), [handleClickName]);

  const renderReceiver = useCallback(({ record }: RenderProps) => {
    const receiverList = record?.get('receiverList') || [];
    return (
      <span>
        {receiverList.map((user: User) => user.realName).join('、')}
      </span>
    );
  }, []);

  const renderAssignee = useCallback(({ record }) => (
    <div style={{ display: 'inline-flex' }}>
      {
        record.get('assigneeId') && record.get('assigneeId') !== '0' && (
          <UserHead
            // @ts-ignore
            user={{
              id: record.get('assigneeId'),
              name: record.get('assigneeName'),
              loginName: record.get('assigneeLoginName'),
              realName: record.get('assigneeRealName'),
              avatar: record.get('assigneeImageUrl'),
            }}
          />
        )
      }
    </div>
  ), []);

  const renderStatus = useCallback(({ value }) => (
    <div className={`${styles.status} ${styles[`status_${value}`]}`}>
      {
        value ? '启用' : '停用'
      }
    </div>
  ), []);

  const renderAction = useCallback(({ dataSet, record }: RenderProps) => {
    const handleDeleteRule = () => {
      pageRuleApi.delete(record?.get('id')).then(() => {
        Choerodon.prompt('删除成功');
        dataSet?.query(dataSet?.toData().length === 1 ? dataSet?.currentPage - 1 : dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('删除失败');
      });
    };

    const handleStartRule = () => {
      pageRuleApi.startRule(record?.get('id')).then(() => {
        Choerodon.prompt('启用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('启用失败');
      });
    };

    const handleStopRule = () => {
      pageRuleApi.stopRule(record?.get('id')).then(() => {
        Choerodon.prompt('停用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('停用失败');
      });
    };

    const handleMenuClick = (e: { key: 'delete' | 'start' | 'stop' }) => {
      switch (e.key) {
        case 'delete': {
          Modal.open({
            style: {
              width: 416,
            },
            key: 'delete',
            title: '删除',
            children: <span>确定要删除该通知规则吗？</span>,
            onOk: handleDeleteRule,
          });
          break;
        }
        case 'start': {
          handleStartRule();
          break;
        }
        case 'stop': {
          handleStopRule();
          break;
        }
        default: {
          break;
        }
      }
    };

    const menu = (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={handleMenuClick.bind(this)}>
        <Menu.Item key="delete">删除</Menu.Item>
        {
          record?.get('enabled') && (
            <Menu.Item key="stop">停用</Menu.Item>
          )
        }
        {
          !record?.get('enabled') && (
            <Menu.Item key="start">启用</Menu.Item>
          )
        }
      </Menu>
    );
    return (
      <Dropdown
        overlay={menu}
        trigger={['click'] as Action[]}
      >
        <Icon
          type="more_vert"
          style={{
            fontSize: 18,
          }}
        />
      </Dropdown>
    );
  }, [tableDataSet]);

  return (
    <Table dataSet={tableDataSet} queryBar={'none' as TableQueryBarType}>
      <Column name="name" renderer={renderName} width={200} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="action" width={50} renderer={renderAction} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="expressQuery" width={250} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="newAssignee" renderer={renderAssignee} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="receiverList" renderer={renderReceiver} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="enabled" renderer={renderStatus} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="source" renderer={({ value }) => (value === 'system' ? '预置' : '自定义')} tooltip={'overflow' as TableColumnTooltip} />
    </Table>
  );
};

export default RuleTable;
