import {
  Button, Icon, Modal, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import { Tree } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import {
  IPublishVersionTreeNode, IPublishVersionData, publishVersionApi, versionApi,
} from '@/api';
import { IAppVersionDataItem } from '@/components/release-detail/stores/store';
import { useReleaseDetailContext } from '../../../stores';
import Section from '../../section';
import './index.less';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';

const { TreeNode } = Tree;
const DependencyTree: React.FC = () => {
  const { disabled, prefixCls, store } = useReleaseDetailContext();
  const delConfigRef = useRef<string>('only');
  const data = store.getDependencyList;
  const detailData = store.getCurrentData;
  function handleLinkPublishVersion(linkData: any) {
    publishVersionApi.dependencyTreeAdd({
      id: detailData.id,
      type: 'app',
      children: linkData?.version.map((item: any) => ({ id: item, type: 'app' })) || [],

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
          type: 'app',
          children: [
            { id: v.id, type: 'app' },
          ],
        }).then(() => {
          store.loadData();
        });
      },
    });
  }

  function renderTreeNode(item: IPublishVersionTreeNode) {
    return (
      <div role="none" className={`${prefixCls}-dependency-tree-item`}>
        <span className={`${prefixCls}-dependency-tree-item-left`}>
          <Icon type="folder-o" className={`${prefixCls}-dependency-tree-item-left-icon`} />
          <span className={`${prefixCls}-dependency-tree-item-left-text`}>{item.versionAlias || item.version}</span>
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
  function renderTree(item: IPublishVersionTreeNode, level = 0) {
    let paddingLeft = level === 0 ? undefined : (28 * level - 20 * (level - 1));
    if (!!item.children?.length && level > 1 && paddingLeft && paddingLeft > 0) {
      paddingLeft -= 11;
    }
    return (
      <TreeNode
        title={renderTreeNode(item)}
        key={item.id}
        disabled
        className={classnames({
          [`${prefixCls}-dependency-tree-tree-root`]: !level,
          [`${prefixCls}-dependency-tree-tree-leaf`]: !item.children?.length,
          [`${prefixCls}-dependency-tree-tree-has-leaf`]: !!item.children?.length,
        })}
        style={{
          paddingLeft,
          marginLeft: level > 1 ? 20 * (level - 1) : undefined,
        }}
        switcherIcon={item.children?.length ? (nodeProps: any) => <div className={`${prefixCls}-dependency-tree-tree-expand`}><Icon type="navigate_next" className={`${prefixCls}-dependency-tree-tree-expand-icon`} /></div>
          : undefined}
      >
        {item.children?.map((k) => renderTree(k, level + 1))}
      </TreeNode>
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
                  openLinkPublishVersionModal({ publishVersionId: detailData.id, handleOk: handleLinkPublishVersion });
                }}
              />
            </Tooltip>

          </div>
        ) : ''
      }
      contentClassName={`${prefixCls}-dependency-tree`}
    >
      <Tree className={`${prefixCls}-dependency-tree-tree`}>

        {data[0]?.children?.map((item) => renderTree(item, 0))}
      </Tree>

    </Section>
  );
};
export default observer(DependencyTree);
