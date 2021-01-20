import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { withRouter } from 'react-router-dom';
import {
  Table, Dropdown, Icon, Menu, Modal, Button,
} from 'choerodon-ui/pro';
import {
  Content, Page, Breadcrumb, Choerodon, Header,
} from '@choerodon/boot';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { IIssueType } from '@/common/types';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { issueTypeApi } from '@/api';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { getIsOrganization } from '@/utils/common';
import TypeTag from '../../../components/TypeTag/TypeTag';
import AddIssueType from './AddIssueType';
import Store from '../stores';
import styles from './IssueTypeList.less';
import openUsage from './Usage';

const { Column } = Table;

/**
 * 问题类型页面
 * 鼠标点击相关方案，出现弹窗是否进入关联的相关方案 待定
 */
function IssueTypeList() {
  const context = useContext(Store);
  const { issueTypeDataSet } = context;
  const isOrganization = getIsOrganization();

  const handleEdit = useCallback(({ record, dataSet }) => {
    Modal.open({
      className: styles.rule_modal,
      drawer: true,
      style: {
        width: 480,
      },
      key: Modal.key(),
      title: '编辑问题类型',
      // @ts-ignore
      children: <AddIssueType typeId={record?.get('id')} typeTableDataSet={dataSet} />,
    });
  }, []);
  /**
   * render Name
   * @param {*} param0
   */
  const renderName = useCallback(({ record, dataSet }: RenderProps) => {
    const colour = record?.get('colour');
    const name = record?.get('name');
    const icon = record?.get('icon');
    const data = {
      colour,
      name,
      icon,
    };
    return (
      <div className={styles.name} role="none" onClick={() => handleEdit({ record, dataSet })}>
        <TypeTag
          data={data as IIssueType}
          showName
          style={{ margin: 0 }}
        />
      </div>
    );
  }, [handleEdit]);

  const renderAction = useCallback(({ dataSet, record }: RenderProps) => {
    const handleDelete = () => {
      issueTypeApi[isOrganization ? 'orgDelete' : 'delete'](record?.get('id')).then(() => {
        Choerodon.prompt('删除成功');
        dataSet?.query(dataSet?.toData().length === 1 ? dataSet?.currentPage - 1 : dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('删除失败');
      });
    };

    const handleReferenced = () => {
      issueTypeApi.referenced(record?.get('id')).then(() => {
        Choerodon.prompt('启用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('启用失败');
      });
    };

    const handleDontReferenced = () => {
      issueTypeApi.dontReferenced(record?.get('id')).then(() => {
        Choerodon.prompt('启用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('启用失败');
      });
    };

    const handleStart = () => {
      issueTypeApi.start(record?.get('id')).then(() => {
        Choerodon.prompt('启用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('启用失败');
      });
    };

    const handleStop = () => {
      issueTypeApi.stop(record?.get('id')).then(() => {
        Choerodon.prompt('停用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('停用失败');
      });
    };

    const handleMenuClick = (e: { key: 'delete' | 'start' | 'stop' | 'referenced' | 'dontReferenced'}) => {
      switch (e.key) {
        case 'delete': {
          Modal.open({
            style: {
              width: 416,
            },
            key: 'delete',
            title: '删除',
            children: <span>确定要删除该问题类型吗？</span>,
            onOk: handleDelete,
          });
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
        case 'referenced': {
          handleReferenced();
          break;
        }
        case 'dontReferenced': {
          handleDontReferenced();
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
        {
          record?.get('scource') !== 'system' && ( // 添加后端返回的是否可删除
            <Menu.Item key="delete">删除</Menu.Item>
          )
        }
        {
          isOrganization && record?.get('referenced') && (
            <Menu.Item key="referenced">不允许引用</Menu.Item>
          )
        }
        {
          isOrganization && !record?.get('referenced') && (
            <Menu.Item key="dontReferenced">允许引用</Menu.Item>
          )
        }
        {
          !isOrganization && record?.get('enabled') && (
            <Menu.Item key="stop">停用</Menu.Item>
          )
        }
        {
          !isOrganization && !record?.get('enabled') && (
            <Menu.Item key="start">启用</Menu.Item>
          )
        }
      </Menu>
    );
    return isOrganization && record?.get('source') === 'system' ? null : (
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
  }, [isOrganization]);

  const renderUsage = useCallback(({ record }: RenderProps) => (
    <div className={styles.usage} role="none" onClick={() => openUsage({ record })}>
      8个关联项目
    </div>
  ), []);

  const renderSource = useCallback(({ record }: RenderProps) => {
    const sourceMap = new Map([
      ['system', '系统'],
      ['organization', '组织'],
      ['project', '项目'],
    ]);
    return sourceMap.get(record?.get('source'));
  }, []);

  const renderStatus = useCallback(({ value }) => (
    <div className={`${styles.status} ${styles[`status_${value}`]}`}>
      {
        value ? '启用' : '停用'
      }
    </div>
  ), []);

  const renderReferenced = useCallback(({ value }) => (
    <div>
      {
        value ? '是' : '否'
      }
    </div>
  ), []);

  const handleAdd = () => {
    Modal.open({
      className: styles.rule_modal,
      drawer: true,
      style: {
        width: 480,
      },
      key: Modal.key(),
      title: '添加问题类型',
      // @ts-ignore
      children: <AddIssueType typeTableDataSet={issueTypeDataSet} />,
    });
  };

  return (
    <Page
      className={styles.issueType}
      service={[
        'choerodon.code.organization.setting.issue.issue-type.ps.default',
      ]}
    >
      <Header>
        <Button icon="playlist_add" onClick={handleAdd}>添加问题类型</Button>
      </Header>
      <Breadcrumb />
      <Content style={{ paddingTop: '0' }}>
        <Table dataSet={issueTypeDataSet} className={styles.issueTypeTable}>
          <Column name="name" renderer={renderName} />
          <Column name="action" width={50} renderer={renderAction} />
          <Column name="description" />
          {
            isOrganization && (
              <Column name="usage" renderer={renderUsage} />
            )
          }
          <Column
            name="source"
            renderer={renderSource}
          />
          {
            isOrganization && (
              <Column name="referenced" renderer={renderReferenced} />
            )
          }
          {
            !isOrganization && (
              <Column name="enabled" renderer={renderStatus} />
            )
          }
        </Table>
      </Content>
    </Page>
  );
}

export default withRouter(observer(IssueTypeList));
