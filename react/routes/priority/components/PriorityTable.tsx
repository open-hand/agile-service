import React, {
  useCallback, useImperativeHandle, useMemo, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, DataSet, Dropdown, Menu, Modal, Form, Select,
} from 'choerodon-ui/pro';
import { Icon, message } from 'choerodon-ui';

import { useIntl } from 'react-intl';
import update from 'immutability-helper';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';

import { ISequence, priorityApi, priorityApiConfig } from '@/api';
import { IPriority } from '@/common/types';
import styles from './PriorityTable.less';
import openPriorityModal from './PriorityModal';

const { Column } = Table;
const { Option } = Select;

const Priority: React.FC<{
  priorityTableRef: React.MutableRefObject<{ query: () => void } | null>
}> = ({ priorityTableRef }) => {
  const intl = useIntl();
  const priorityDs = useMemo(() => new DataSet({
    selection: false,
    autoQuery: true,
    paging: false,
    transport: {
      read: ({ data }) => priorityApiConfig.load(data),
    },
    fields: [{
      name: 'name',
      type: 'string' as FieldType,
      label: '名称',
    }, {
      name: 'description',
      type: 'string' as FieldType,
      label: '描述',
    }, {
      name: 'color',
      type: 'string' as FieldType,
      label: '颜色',
    }],
    queryFields: [{
      name: 'name',
      label: '名称',
    }, {
      name: 'description',
      label: '描述',
    }],
  }), []);

  useImperativeHandle(priorityTableRef, () => ({
    query: () => priorityDs.query(),
  }));

  const renderName = useCallback(({ record, text }: RenderProps) => {
    let name = text;
    if (record?.get('default')) {
      name = `${text} ${intl.formatMessage({ id: 'priority.default' })}`;
    }
    return name;
  }, [intl]);

  const renderAction = useCallback(({ dataSet, record }: RenderProps) => {
    let priorityId: string | undefined;
    function setPriorityId(val: any) {
      priorityId = val;
    }
    // @ts-ignore
    const enableList = priorityDs.toData().filter((item) => item.enable);
    const handleDelete = async () => {
      const count = await priorityApi.checkBeforeDel(record?.get('id'));
      // @ts-ignore
      const priorityList: { id: string, name: string }[] = priorityDs.toData().filter((item) => item.id !== record?.get('id'));

      const deletePriority = async (id: string, defaultId: string) => {
        try {
          await priorityApi.delete(id, priorityId || defaultId);
          priorityDs.query();
          setPriorityId(undefined);
          return true;
        } catch (err) {
          message.error('删除失败');
          return false;
        }
      };

      const handleSelectChange = (id: string) => {
        setPriorityId(id);
      };
      Modal.open({
        title: intl.formatMessage({ id: 'priority.delete.title' }),
        children: (
          <div>
            <div style={{ marginBottom: 10 }}>
              {`${intl.formatMessage({ id: 'priority.delete.title' })}：${record?.get('name')}`}
            </div>
            {count !== 0
              && (
                <div style={{ marginBottom: 10 }}>
                  <Icon
                    type="error"
                    style={{
                      verticalAlign: 'top',
                      color: 'red',
                      marginRight: 5,
                    }}
                  />
                  {intl.formatMessage({ id: 'priority.delete.used.tip.prefix' })}
                  <span style={{ color: 'red' }}>{count}</span>
                  {intl.formatMessage({ id: 'priority.delete.used.tip.suffix' })}
                </div>
              )}
            <div style={{ marginBottom: 15 }}>
              {intl.formatMessage({ id: 'priority.delete.notice' })}
              {count !== 0 && intl.formatMessage({ id: 'priority.delete.used.notice' })}
            </div>
            {count !== 0
              && (
                <Form style={{ marginBottom: -20 }}>
                  <Select
                    label={intl.formatMessage({ id: 'priority.title' })}
                    placeholder={intl.formatMessage({ id: 'priority.delete.chooseNewPriority.placeholder' })}
                    onChange={handleSelectChange}
                    style={{ width: 470 }}
                    defaultValue={priorityList[0].id}
                    clearButton={false}
                  >
                    {priorityList.map(
                      (item) => <Option value={item.id} key={String(item.id)}>{item.name}</Option>,
                    )}
                  </Select>
                </Form>
              )}
          </div>),
        width: 520,
        onOk() {
          return deletePriority(record?.get('id'), priorityList[0].id);
        },
        onCancel() {
          setPriorityId(undefined);
          return true;
        },
        okText: '删除',
        cancelText: '取消',
      });
    };

    const handleStart = async () => {
      try {
        await priorityApi.updateStatus(record?.get('id'), true);
        priorityDs.query();
        return true;
      } catch (err) {
        message.error('修改状态失败');
        return false;
      }
    };

    const handleStop = () => {
      Modal.open({
        title: intl.formatMessage({ id: 'priority.disable.title' }),
        children: (
          <div>
            <div style={{ marginBottom: 10 }}>
              {intl.formatMessage({ id: 'priority.disable.title' })}
              :
              {record?.get('name')}
            </div>
            <div>{intl.formatMessage({ id: 'priority.disable.notice' })}</div>
          </div>),
        onOk: async () => {
          try {
            await priorityApi.updateStatus(record?.get('id'), false);
            priorityDs.query();
            return true;
          } catch (err) {
            message.error('修改状态失败');
            return false;
          }
        },
        okText: '确认',
        cancelText: '取消',
      });
    };

    const queryTable = () => {
      priorityDs.query();
    };

    const handleMenuClick = (e: { key: 'edit' | 'delete' | 'start' | 'stop' }) => {
      switch (e.key) {
        case 'edit': {
          openPriorityModal({ onOk: queryTable, priorityId: record?.get('id'), editingPriority: record?.data as (IPriority | undefined) });
          break;
        }
        case 'delete': {
          handleDelete();
          break;
        }
        case 'start': {
          handleStart();
          break;
        }
        case 'stop': {
          handleStop();
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
        <Menu.Item key="edit">修改</Menu.Item>
        {
          enableList.length > 1 && (
            <Menu.Item key="delete">删除</Menu.Item>
          )
        }
        {
          record?.get('enable') && enableList.length > 1 && (
            <Menu.Item key="stop">停用</Menu.Item>
          )
        }
        {
          !record?.get('enable') && (
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
            cursor: 'pointer',
            color: 'var(--primary-color)',
          }}
        />
      </Dropdown>
    );
  }, [intl, priorityDs]);

  const renderColor = useCallback(({ record }) => (
    <span style={{ display: 'inline-flex', alignItems: 'center', height: '100%' }}>
      <div style={{
        flexGrow: 0,
        width: 16,
        height: 16,
        borderRadius: 3,
        backgroundColor: record?.get('colour'),
      }}
      />
    </span>
  ), []);

  const handleDragEnd = useCallback((dataSet, columns, resultDrag, provided) => {
    const { source, destination } = resultDrag;
    if (!destination) {
      return;
    }
    if (source.index === destination.index) {
      return;
    }
    const priorityList: ISequence[] = priorityDs.toData() as ISequence[];
    const dragRow: any = priorityList[destination.index];
    if (!dragRow.enable) {
      priorityDs.query();
      return;
    }
    priorityApi.sort(priorityList.map((item) => ({
      id: item.id,
      sequence: item.sequence,
    }))).then(() => {
      priorityDs.query();
    });
  }, [priorityDs]);

  return (
    <Table
      dataSet={priorityDs}
      onRow={({ record }) => ({
        className: !record?.get('enable') ? styles.disabled_row : '',
      })}
      rowDraggable
      onDragEnd={handleDragEnd}
      pristine
    >
      <Column name="name" renderer={renderName} />
      <Column name="action" renderer={renderAction} />
      <Column name="description" />
      <Column name="color" renderer={renderColor} />
    </Table>
  );
};

export default observer(Priority);
