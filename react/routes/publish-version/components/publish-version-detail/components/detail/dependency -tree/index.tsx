import {
  Button, Icon, Modal, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import { Tree } from 'choerodon-ui';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { IAppVersionData, versionApi } from '@/api';
import { IAppVersionDataItem } from '@/components/release-detail/stores/store';
import { useReleaseDetailContext } from '../../../stores';
import Section from '../../section';
import './index.less';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';

const { TreeNode } = Tree;
const DependencyTree: React.FC = () => {
  const { disabled, prefixCls, store } = useReleaseDetailContext();
  const delConfigRef = useRef<string>('only');
  const data = store.getAppServiceList;
  const detailData = store.getCurrentData;
  function handleLinkAppService(linkData: any) {
    versionApi.linkAppVersions(detailData.versionId, linkData.version).then(() => {
      store.loadData();
    });
  }
  function handleDelete(v: IAppVersionData) {
    Modal.confirm({
      title: '删除应用版本',
      children: (
        <div>
          <span>{`您确定要删除关联的应用版本【${v.versionAlias || v.version}】？`}</span>
          <SelectBox mode={'box' as any} defaultValue="only" onChange={(value: any) => { delConfigRef.current = value; }}>
            <SelectBox.Option value="only">仅删除关联关系</SelectBox.Option>
            <SelectBox.Option value="all">删除关联关系及应用版本</SelectBox.Option>
          </SelectBox>
        </div>),
      onOk: () => {
        (delConfigRef.current === 'only' ? versionApi.deleteLinkAppVersion(detailData.versionId, v.id!) : versionApi.deleteAppVersion(v.id!)).then(() => {
          store.loadData();
        });
      },
    });
  }

  function renderTreeNode(item: IAppVersionDataItem) {
    return (
      <div role="none" className={`${prefixCls}-dependency-tree-item`}>
        <span className={`${prefixCls}-dependency-tree-item-left`}>
          <Icon type="folder-o" className={`${prefixCls}-dependency-tree-item-left-icon`} />
          <span className={`${prefixCls}-dependency-tree-item-left-text`}>{item.name || `${item.artifactId}/${item.versionAlias || item.version}`}</span>
        </span>

        <Button
          icon="delete_forever"
          className={`${prefixCls}-dependency-tree-item-btn`}
          onClick={(e) => {
            e.stopPropagation();
            handleDelete(item);
          }}
        />
      </div>
    );
  }
  return (
    <Section
      title="关联发布版本"
      buttons={
        !disabled ? (
          <div className={`${prefixCls}-dependency-tree-operation`}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="关联发布版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                icon="playlist_add"
                onClick={() => {
                  openLinkPublishVersionModal({});
                }}
              />
            </Tooltip>

          </div>
        ) : ''
      }
      contentClassName={`${prefixCls}-dependency-tree`}
    >
      <Tree className={`${prefixCls}-dependency-tree-tree`}>
        {data.map((item) => (
          <TreeNode title={renderTreeNode(item)} key={item.id}>
            {item.children?.flatMap((k) => <TreeNode title={renderTreeNode(k)} />)}
          </TreeNode>
        ))}
      </Tree>

    </Section>
  );
};
export default observer(DependencyTree);
