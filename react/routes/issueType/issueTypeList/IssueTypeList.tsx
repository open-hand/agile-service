import React, { useCallback, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { withRouter } from 'react-router-dom';
import {
  Dropdown, Icon, Menu, Modal, Table,
} from 'choerodon-ui/pro';
import {
  Breadcrumb, Choerodon, Content, Header, Page,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { ColumnAlign } from 'choerodon-ui/pro/lib/table/enum';
import { Tooltip } from 'choerodon-ui/pro/lib/core/enum';
import { IIssueType } from '@/common/types';
import { issueTypeApi } from '@/api';
import TypeTag from '../../../components/TypeTag/TypeTag';
import AddIssueType from './AddIssueType';
import Store from '../stores';
import styles from './IssueTypeList.less';
import openUsage from './Usage';
import openLink from './LinkType';
import useFormatMessage from '@/hooks/useFormatMessage';
import useIsProgram from '@/hooks/useIsProgram';
import useIsWaterfall from '@/hooks/useIsWaterfall';

const { Column } = Table;

function IssueTypeList() {
  const context = useContext(Store);
  const { issueTypeDataSet, isOrganization, hiddenActionTypeCodes } = context;
  const formatMessage = useFormatMessage();
  const { isWaterfall, isWaterfallAgile } = useIsWaterfall();
  const { isAgileProgram } = useIsProgram();
  const handleEdit = useCallback(({ record, dataSet }) => {
    Modal.open({
      className: styles.issueType_modal,
      drawer: true,
      style: {
        width: 480,
      },
      key: Modal.key(),
      title: '修改工作项类型',
      children: <AddIssueType typeId={record?.get('id')} typeTableDataSet={dataSet} isOrganization={isOrganization} />,
      okText: '保存',
      footer: null,
    });
  }, [isOrganization]);
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
      <div className={styles.name}>
        {isOrganization ? null : <Icon type="baseline-drag_indicator" className={styles.name_drag} />}
        <TypeTag
          data={data as IIssueType}
          showName
          style={{ margin: 0 }}
        />
      </div>
    );
  }, []);

  const renderAction = useCallback(({ dataSet, record }: RenderProps) => {
    if (!isOrganization && isAgileProgram && hiddenActionTypeCodes.includes(record?.get('typeCode'))) {
      return null;
    }
    const handleDelete = () => {
      issueTypeApi[isOrganization ? 'orgDelete' : 'delete'](record?.get('id')).then(() => {
        Choerodon.prompt('删除成功');
        dataSet?.query(dataSet?.toData().length === 1 ? dataSet?.currentPage - 1 : dataSet?.currentPage);
      }).catch((error: any = {}) => {
        Choerodon.prompt(error.message ?? '删除失败');
      });
    };

    const handleReferenced = () => {
      issueTypeApi.orgReferenced(record?.get('id'), true).then(() => {
        Choerodon.prompt('允许引用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('允许引用失败');
      });
    };

    const handleDontReferenced = () => {
      issueTypeApi.orgReferenced(record?.get('id'), false).then(() => {
        Choerodon.prompt('不允许引用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('不允许引用失败');
      });
    };

    const handleStart = () => {
      issueTypeApi.enabled(record?.get('id'), true).then(() => {
        Choerodon.prompt('启用成功');
        dataSet?.query(dataSet?.currentPage);
      }).catch(() => {
        Choerodon.prompt('启用失败');
      });
    };

    const handleStop = () => {
      issueTypeApi.getStopDisable(record?.get('id')).then((disable: boolean) => {
        if (!disable) {
          Choerodon.prompt(`至少启用一个${record?.get('typeCode') === 'sub_task' ? '子级' : '父级'}工作项类型`);
        } else {
          issueTypeApi.enabled(record?.get('id'), false).then(() => {
            Choerodon.prompt('停用成功');
            dataSet?.query(dataSet?.currentPage);
          }).catch(() => {
            Choerodon.prompt('停用失败');
          });
        }
      });
    };

    const handleMenuClick = (e: { key: 'edit' | 'delete' | 'start' | 'stop' | 'referenced' | 'dontReferenced' }) => {
      switch (e.key) {
        case 'edit': {
          handleEdit({ record, dataSet });
          break;
        }
        case 'delete': {
          Modal.open({
            className: styles.delete_modal,
            style: {
              width: 416,
            },
            key: 'delete',
            title: '删除工作项类型',
            children: <span>{`确定要删除“${record?.get('name')}”工作项类型？删除后，该工作项类型的页面字段方案和状态机方案将一并删除。`}</span>,
            onOk: handleDelete,
            border: false,
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
      <Menu onClick={handleMenuClick.bind(this)} className={styles.issueType_menu}>
        <Menu.Item key="edit">{formatMessage({ id: 'boot.modify' })}</Menu.Item>
        {
          record?.get('deleted') && (
            <Menu.Item key="delete">{formatMessage({ id: 'boot.delete' })}</Menu.Item>
          )
        }
        {
          isOrganization && record?.get('source') !== 'system' && record?.get('referenced') && (
            <Menu.Item key="dontReferenced">不允许引用</Menu.Item>
          )
        }
        {
          isOrganization && record?.get('source') !== 'system' && !record?.get('referenced') && (
            <Menu.Item key="referenced">允许引用</Menu.Item>
          )
        }
        {
          !isOrganization && record?.get('enabled') && (
            <Menu.Item key="stop">
              {formatMessage({ id: 'boot.disable' })}
            </Menu.Item>
          )
        }
        {
          !isOrganization && !record?.get('enabled') && (
            <Menu.Item key="start">{formatMessage({ id: 'boot.enable' })}</Menu.Item>
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
  }, [handleEdit, isOrganization]);

  const renderTypeCode = useCallback(({ record }: RenderProps) => {
    const standardTypeMap = new Map([
      ['story', '故事'],
      ['task', '任务'],
      ['bug', '缺陷'],
      ['sub_task', '子任务'],
    ]);
    return record?.get('source') !== 'system' ? standardTypeMap.get(record?.get('typeCode')) : '-';
  }, []);

  const renderUsage = useCallback(({ record }: RenderProps) => (
    <div className={styles.usage} role="none" onClick={() => openUsage({ record })}>
      {record?.get('usageCount') ? `${record?.get('usageCount')}个关联项目` : '无'}
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
  const handleRefresh = () => {
    issueTypeDataSet.query(issueTypeDataSet.currentPage);
  };

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
      className: styles.issueType_modal,
      drawer: true,
      style: {
        width: 480,
      },
      key: Modal.key(),
      title: '添加工作项类型',
      children: <AddIssueType typeTableDataSet={issueTypeDataSet} isOrganization={isOrganization} />,
      okText: '保存',
      footer: null,
    });
  };

  const handleOpenRefrenced = useCallback(() => {
    openLink({ issueTypeDataSet });
  }, [issueTypeDataSet]);
  return (
    <Page
      className={styles.issueType}
    >
      <Header>
        <HeaderButtons items={[
          {
            name: formatMessage({ id: 'agile.issueType.add' }),
            icon: 'playlist_add',
            handler: handleAdd,
            display: isWaterfall ? isWaterfallAgile : true,
          }, {
            name: formatMessage({ id: 'agile.issueType.reference' }),
            icon: 'relate',
            handler: handleOpenRefrenced,
            display: isWaterfall ? isWaterfallAgile : !isOrganization,
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Content>
        <Table
          dataSet={issueTypeDataSet}
          className={styles.issueTypeTable}
          rowDraggable={!isOrganization}
          onDragEnd={async (ds, columns, resultDrag) => {
            const { draggableId, destination, source: { index: sourceIndex } } = resultDrag;
            if (!destination) {
              return;
            }
            const { index: destinationIndex } = destination;
            if (destinationIndex === sourceIndex) {
              return;
            }
            const dragIssueTypeId = ds.findRecordById(draggableId)?.get('id');
            const frontId = ds.get(destinationIndex - 1)?.get('id');
            const backId = ds.get(destinationIndex + 1)?.get('id');
            if (!dragIssueTypeId || (!frontId && !backId)) {
              return;
            }
            await issueTypeApi.updateRank(dragIssueTypeId, { frontId, backId });
            handleRefresh();
          }}
        >
          <Column name="name" width={150} renderer={renderName} />
          <Column name="action" width={50} renderer={renderAction} />
          <Column name="description" tooltip={Tooltip.overflow} />
          <Column name="typeCode" renderer={renderTypeCode} width={150} />
          {
            isOrganization && (
              <Column name="usage" renderer={renderUsage} />
            )
          }
          <Column
            name="source"
            renderer={renderSource}
            width={100}
          />
          {
            isOrganization && (
              <Column name="referenced" renderer={renderReferenced} />
            )
          }
          {
            !isOrganization && (
              <Column name="enabled" renderer={renderStatus} width={150} align={ColumnAlign.left} />
            )
          }
        </Table>
      </Content>
    </Page>
  );
}

export default withRouter(observer(IssueTypeList));
