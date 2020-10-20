/* eslint-disable jsx-a11y/click-events-have-key-events */
import React, { useMemo } from 'react';
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
import styles from '../../index.less';
import RuleModal from '../rule-modal';

const { Column } = Table;

interface Props{
    tableDataSet: DataSet,
}

const RuleTable: React.FC<Props> = ({ tableDataSet }) => {
  const handleClickReceiver = (ruleId: string) => {
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
  };
  const renderReceiver = ({ record }: RenderProps) => {
    const receiverList = record?.get('receiverList') || [];
    return (
      // eslint-disable-next-line jsx-a11y/no-static-element-interactions
      <span
        className={styles.receiver}
        onClick={() => handleClickReceiver(record?.get('id'))}
      >
        {receiverList.map((user: User) => user.realName).join('、')}
      </span>
    );
  };

  const renderCcList = ({ record }: RenderProps) => {
    const receiverList = record?.get('ccList') || [];
    return (
      <span>
        {receiverList.map((user: User) => user.realName).join('、')}
      </span>
    );
  };

  const renderAction = ({ record }: RenderProps) => {
    const handleDeleteRule = () => {
      axios.delete(`/agile/v1/projects/${getProjectId()}/configuration_rule/${record?.get('id')}`).then(() => {
        Choerodon.prompt('删除成功');
        tableDataSet.query(tableDataSet.toData().length === 1 ? tableDataSet.currentPage - 1 : tableDataSet.currentPage);
      }).catch(() => {
        Choerodon.prompt('删除失败');
      });
    };
    const handleMenuClick = () => {
      Modal.open({
        style: {
          width: 416,
        },
        key: 'delete',
        title: '删除',
        children: <span>确定要删除该通知规则吗？</span>,
        onOk: handleDeleteRule,
      });
    };

    const menu = (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={handleMenuClick.bind(this, record)}>
        <Menu.Item key="delete">删除</Menu.Item>
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
  };

  return (
    <Table dataSet={tableDataSet} queryBar={'none' as TableQueryBarType}>
      <Column name="receiverList" width={250} renderer={renderReceiver} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="action" width={50} renderer={renderAction} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="ccList" width={250} renderer={renderCcList} tooltip={'overflow' as TableColumnTooltip} />
      <Column name="expressQuery" tooltip={'overflow' as TableColumnTooltip} />
    </Table>
  );
};

export default RuleTable;
