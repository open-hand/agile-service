import React, { Component, useState } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Content, stores, Permission, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import {
  Menu, Icon, Spin, Tooltip,
} from 'choerodon-ui';
import { Table, DataSet } from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { versionApi } from '@/api';
import TableDropMenu from '@/components/table-drop-menu';
import AddRelease from '../ReleaseComponent/AddRelease';
import ReleaseStore from '../../../stores/project/release/ReleaseStore';
import './ReleaseHome.less';
import EditRelease from '../ReleaseComponent/EditRelease';
import PublicRelease from '../ReleaseComponent/PublicRelease';
import DeleteReleaseWithIssues from '../ReleaseComponent/DeleteReleaseWithIssues';
import { openLinkVersionModal } from '../ReleaseComponent/link-program-vesion';
import { openCreatReleaseVersionModal, openEditReleaseVersionModal } from '../components/create-edit-release-version';

const { Column } = Table;
const { AppState } = stores;
const COLOR_MAP = {
  规划中: '#ffb100',
  已发布: '#00bfa5',
  归档: 'rgba(0, 0, 0, 0.3)',
};
interface ReleaseHomeProps {
  isInProgram: boolean
  tableDataSet: DataSet
  program?: { id: string }
}
const ReleaseHome: React.FC<ReleaseHomeProps> = ({ isInProgram, tableDataSet, program }) => {
  const [createVisible, setCreateVisible] = useState(false);
  const [editVersionData, setEditVersionData] = useState<any>();
  const [versionDelInfo, setVersionDelInfo] = useState<any>();
  const [publicVersionVisible, setPublicVersionVisible] = useState(false);
  function handleRefresh() {
    tableDataSet.query(tableDataSet.currentPage);
  }
  function handleClickMenu(record: Record, key: string) {
    const recordData = record.toData();
    switch (key) {
      case 'releaseStatus': {
        if (recordData.statusCode === 'version_planning') {
          versionApi.loadPublicVersionDetail(recordData.versionId)
            .then((res: any) => {
              ReleaseStore.setPublicVersionDetail(res);
              ReleaseStore.setVersionDetail(recordData);
              setPublicVersionVisible(true);
            }).catch(() => {
            });
        } else {
          versionApi.revokePublish(
            recordData.versionId,
          ).then(() => {
            handleRefresh();
          }).catch(() => {
          });
        }
        break;
      }
      case 'del': {
        versionApi.loadNamesAndIssueBeforeDel(recordData.versionId).then((res: any) => {
          setVersionDelInfo({
            versionName: recordData.name,
            versionId: recordData.versionId,
            ...res,
          });

          ReleaseStore.setDeleteReleaseVisible(true);
        }).catch(() => {
        });
        break;
      }
      case 'edit': {
        versionApi.load(recordData.versionId).then((res: any) => {
          // ReleaseStore.setVersionDetail(res);
          // setEditVersionData(res);
          openEditReleaseVersionModal({ editData: res, handleOk: () => tableDataSet.query() });
        });
        break;
      }
      case 'archivedStatus': {
        if (recordData.statusCode === 'archived') {
          // 撤销归档
          versionApi.revokeArchived(recordData.versionId).then(() => {
            handleRefresh();
          }).catch(() => {
          });
        } else {
          // 归档
          versionApi.archived(recordData.versionId).then(() => {
            handleRefresh();
          }).catch(() => {
          });
        }
        break;
      }
      case 'linkProgramVersion': {
        openLinkVersionModal(recordData.versionId, program?.id!, recordData.programVersionInfoVOS ? recordData.programVersionInfoVOS[0] : undefined, () => handleRefresh());
        break;
      }
      default:
        break;
    }
  }

  function renderMenu({ text, record }: RenderProps) {
    const { type, id, organizationId } = AppState.currentMenuType;
    const statusCode = record?.get('statusCode');

    return (
      <TableDropMenu
        text={text}
        style={{ lineHeight: 'inherit' }}
        menuData={[
          { action: () => handleClickMenu(record!, 'edit'), text: '编辑' },
          {
            action: () => handleClickMenu(record!, 'releaseStatus'),
            text: statusCode === 'version_planning' ? '发布' : '撤销发布',
            display: statusCode !== 'archived',
            service: ['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversionstatus'],
          },
          {
            action: () => handleClickMenu(record!, 'archivedStatus'),
            text: statusCode === 'archived' ? '撤销归档' : '归档',
            service: ['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversionstatus'],
          },
          {
            action: () => handleClickMenu(record!, 'del'),
            text: '删除',
            display: statusCode !== 'archived',
            service: ['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.deleteversion'],
          },
        ]}
        permissionType={type}
        organizationId={organizationId}
        tooltip
        permissionMenu={{
          service: [
            'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.deleteversion',
            'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversionstatus',
          ],
        }}

      />
    );
  }

  function renderProgramVersion({ value }: RenderProps) {
    if (value && [...(value || undefined)].length > 0) {
      const versionTags = [
        <Tooltip title={value[0].name}><div className="c7n-release-progress-version">{value[0].name}</div></Tooltip>,
      ];
      value.length > 1 && versionTags.push(<Tooltip title={value.slice(1).map((item: any) => item.name).join(',')}><div className="c7n-release-progress-version">...</div></Tooltip>);
      return versionTags;
    }
    return '';
  }
  return (
    <Page>
      <Permission service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.createversion']}>
        <Header title="版本管理">
          <HeaderButtons
            items={[
              {
                name: '创建规划版本',
                icon: 'playlist_add',
                handler: () => {
                  // setCreateVisible(true);
                  openCreatReleaseVersionModal({ handleOk: () => tableDataSet.query() });
                },
                display: true,
              },
            ]}
          />
        </Header>
      </Permission>
      <Breadcrumb />
      <Content className="c7n-release-content">
        <Table
          dataSet={tableDataSet}
          rowDraggable
          onDragEndBefore={(ds, columns, resultDrag) => {
            const currentRecord = ds.findRecordById(resultDrag.draggableId);
            if (!resultDrag.destination || !currentRecord) {
              return false;
            }
            let beforeSequence = null;
            let afterSequence = null;
            if (resultDrag.destination.index > resultDrag.source.index) {
              beforeSequence = ds.get(resultDrag.destination.index)?.get('sequence');
              afterSequence = ds.get(resultDrag.destination.index + 1)?.get('sequence');
            } else {
              afterSequence = ds.get(resultDrag.destination.index)?.get('sequence');
              beforeSequence = ds.get(resultDrag.destination.index - 1)?.get('sequence');
            }
            versionApi.drag({
              afterSequence,
              beforeSequence,
              versionId: currentRecord.get('versionId'),
              objectVersionNumber: currentRecord?.get('objectVersionNumber'),
            })
              .then(() => {
                ds.query(ds.currentPage);
                // this.refresh(pagination);
              }).catch(() => {
                ds.query(ds.currentPage);

                // this.refresh(pagination);
              });
            return true;
          }}
        >
          <Column name="name" renderer={renderMenu} width={180} />
          <Column
            name="status"
            width={100}
            renderer={({ text }) => (
              <div style={{ display: 'flex', alignItems: 'center', height: '100%' }}>
                <span
                  style={{
                    color: '#fff',
                    background: COLOR_MAP[text as keyof typeof COLOR_MAP],
                    display: 'inline-block',
                    lineHeight: '16px',
                    height: '16px',
                    borderRadius: '2px',
                    padding: '0 2px',
                    fontSize: '13px',
                  }}
                >
                  <div style={{ transform: 'scale(.8)' }}>{text === '归档' ? '已归档' : text}</div>
                </span>
              </div>
            )}
          />
          {/* @ts-ignore */}
          <Column name="startDate" className="c7n-agile-table-cell" width={100} tooltip={'overflow' as any} renderer={({ text }) => (text ? text.split(' ')[0] : '')} />
          {/* @ts-ignore */}
          <Column name="expectReleaseDate" className="c7n-agile-table-cell" width={120} tooltip={'overflow' as any} renderer={({ text }) => (text ? text.split(' ')[0] : '')} />
          {/* @ts-ignore */}
          <Column name="releaseDate" className="c7n-agile-table-cell" width={120} tooltip={'overflow' as any} renderer={({ text }) => (text ? text.split(' ')[0] : '')} />
          <Column
            name="description"
            className="c7n-agile-table-cell"
            renderer={({ text }) => (text ? (
              <Tooltip mouseEnterDelay={0.5} title={`描述：${text}`}>
                {text}
              </Tooltip>
            ) : null)}
          />
        </Table>
        {createVisible
          ? (
            <AddRelease
              visible={createVisible}
              onCancel={() => {
                setCreateVisible(false);
              }}
              refresh={handleRefresh}
            />
          ) : null}
        {versionDelInfo ? (
          <DeleteReleaseWithIssues
            visible={versionDelInfo}
            versionDelInfo={versionDelInfo}
            onCancel={() => {
              setVersionDelInfo(undefined);
              ReleaseStore.setDeleteReleaseVisible(false);
            }}
            refresh={handleRefresh}
          />
        ) : null}
        {editVersionData ? (
          <EditRelease
            visible={editVersionData}
            onCancel={() => {
              setEditVersionData(undefined);
            }}
            refresh={handleRefresh}
            data={editVersionData}
          />
        ) : ''}
        {publicVersionVisible ? (
          <PublicRelease
            visible={publicVersionVisible}
            release={tableDataSet.current?.toData()}
            onCancel={() => {
              setPublicVersionVisible(false);
            }}
            refresh={handleRefresh}
          />
        ) : null}
      </Content>
    </Page>
  );
};

export default observer(ReleaseHome);
