import {
  Button, Icon, Modal, Tooltip,
} from 'choerodon-ui/pro/lib';
import { Tree } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import {
  IPublishVersionTreeNode, publishVersionApi,
} from '@/api';
import CustomIcon from '@/components/custom-icon';
import { useReleaseDetailContext } from '../../../stores';
import Section from '../../section';
import './index.less';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';
import { openLinkServiceModal } from '../../link-service-modal';
import { openImportPomModal } from '../../import-pom';
import { openEditAppVersionModal, openCreateAppVersionModal } from './EditAppVersionModal';
import DependencyTreeBase from './DependencyTreeBase';

const { TreeNode } = Tree;
const DependencyTree: React.FC = () => {
  const { disabled, prefixCls, store } = useReleaseDetailContext();
  const delConfigRef = useRef<string>('only');
  const data = store.getDependencyList;
  const detailData = store.getCurrentData;
  function handleLinkPublishVersion(linkData: any) {
    publishVersionApi.dependencyTreeAdd({
      id: detailData.id,
      type: 'publish',
      children: linkData?.version.map((item: any) => ({ id: item, type: 'publish' })) || [],

    }).then(() => {
      store.loadData();
    });
  }
  function handleDelete(v: IPublishVersionTreeNode) {
    Modal.confirm({
      title: '删除发布版本',
      children: (
        <div>
          <span>{`您确定要删除关联的发布版本【${v.versionAlias || v.version}】？`}</span>
          {/* <SelectBox mode={'box' as any} defaultValue="only" onChange={(value: any) => { delConfigRef.current = value; }}>
            <SelectBox.Option value="only">仅删除关联关系</SelectBox.Option>
            <SelectBox.Option value="all">删除关联关系及应用版本</SelectBox.Option>
          </SelectBox> */}
        </div>),
      onOk: () => {
        publishVersionApi.dependencyTreeDel({
          id: detailData.id,
          type: 'publish',
          children: [
            { id: v.id, type: 'publish' },
          ],
        }).then(() => {
          store.loadData();
        });
      },
    });
  }

  function renderTreeNode(item: IPublishVersionTreeNode, level: number) {
    let name = item.versionAlias || item.version;
    name = item.name ? `${item.name}:${name}` : name;
    return (
      <div role="none" className={`${prefixCls}-dependency-tree-item`}>
        <span className={`${prefixCls}-dependency-tree-item-left`}>
          <Icon type="folder-o" className={`${prefixCls}-dependency-tree-item-left-icon`} />
          <span className={`${prefixCls}-dependency-tree-item-left-text`}>{name}</span>
        </span>

        {level === 0 ? (
          <>
            <Button
              icon="mode_edit"
              className={`${prefixCls}-dependency-tree-item-btn`}
              onClick={(e) => {
                e.stopPropagation();
                openEditAppVersionModal({
                  data: item as any,
                  handleOk: async () => {
                    store.loadData();
                    return true;
                  },
                });
              }}
            />
            <Button
              icon="delete_forever"
              className={`${prefixCls}-dependency-tree-item-btn`}
              onClick={(e) => {
                e.stopPropagation();
                handleDelete(item);
              }}
            />
          </>
        ) : null}
      </div>
    );
  }
  function handleLinkAppService(linkData: any) {
    // publishVersionApi.linkAppVersions(detailData.versionId, linkData.version).then(() => {
    //   store.loadData();
    // });
  }
  async function handleImportPom(pomData: any) {
    await publishVersionApi.createBatch(detailData.id, pomData);
    store.loadData();
    return true;
  }
  async function handleCreate(pomData: any) {
    await publishVersionApi.createBatch(detailData.id, [{ ...pomData, appService: false }]);
    store.loadData();
    return true;
  }
  return (
    <Section
      title="关联发布版本"
      buttons={
        !disabled ? (
          <div className={`${prefixCls}-dependency-tree-operation`}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="创建应用版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'primary' as ButtonColor}
                icon="playlist_add"
                onClick={() => {
                  openCreateAppVersionModal({ handleOk: handleCreate });
                }}
              />
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="导入pom文件">
              <Button
                style={{ padding: '0 6px' }}
                color={'primary' as ButtonColor}
                //   icon="mode_edit"
                onClick={() => {
                  openImportPomModal({ handleOk: handleImportPom, versionId: detailData.id });
                }}
              >
                <CustomIcon type="icon-pom" width={18} height={18} />
                {/* {svg} */}
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="关联发布版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'primary' as ButtonColor}
                icon="playlist_add"
                onClick={() => {
                  openLinkPublishVersionModal({ publishVersionId: detailData.id, handleOk: handleLinkPublishVersion });
                }}
              />
            </Tooltip>
            {/* <Tooltip placement="topRight" autoAdjustOverflow={false} title="关联应用版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'primary' as ButtonColor}
                icon="local_offer"
                onClick={() => {
                  openLinkServiceModal({ versionId: detailData.id || detailData.versionId, handleOk: handleLinkAppService });
                }}
              />
            </Tooltip> */}
          </div>
        ) : ''
      }
      border={false}
      contentClassName={`${prefixCls}-dependency-tree`}
    >
      <DependencyTreeBase
        data={data[0]?.children || []}
        renderNode={renderTreeNode}
      />

    </Section>
  );
};
export default observer(DependencyTree);
