import React, { useMemo } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal, Icon,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import {
  IPublishVersionTreeNode, publishVersionApi,
  versionApi,
} from '@/api';
import CustomIcon from '@/components/custom-icon';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Tag } from '@choerodon/components';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { getProjectId } from '@/utils/common';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';
import { openLinkAppServiceTagModal } from './LinkAppServiceTagModal';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';
import { openEditAppVersionModal } from './EditAppVersionModal';
import styles from './index.less';
import DependencyTreeBase, { DependencyTreeNode } from '../dependency-tree-base';
import { openImportPomModal } from '../../../publish-version-detail/components/import-pom';

function PublishVersionLinkVersion() {
  const { store } = usePublishVersionContext();
  const dependencyList = useMemo(() => store.getDependencyList[0]?.children || [], [store.getDependencyList]);
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
      title: '删除关联版本',
      children: (
        <div>
          <span>{`您确定要删除关联的版本【${v.versionAlias || v.version}】？`}</span>
          {/* <SelectBox mode={'box' as any} defaultValue="only" onChange={(value: any) => { delConfigRef.current = value; }}>
            <SelectBox.Option value="only">仅删除关联关系</SelectBox.Option>
            <SelectBox.Option value="all">删除关联关系及应用版本</SelectBox.Option>
          </SelectBox> */}
        </div>),
      onOk: () => {
        (v.type === 'tag' ? publishVersionApi.dependencyTreeDelTag(detailData.id, [v as any])
          : publishVersionApi.dependencyTreeDel({
            id: detailData.id,
            type: 'publish',
            children: [
              { id: v.id, type: v.type },
            ],
          })).then(() => {
          store.loadData();
        });
      },
    });
  }
  function renderTreeNode(item: IPublishVersionTreeNode, level: number) {
    let name = item.versionAlias || item.version;
    const showAdditionalLine = !!(item.groupId || item.artifactId || item.version);
    const appService = item.appServiceCode ? store.findAppServiceByCode(item.appServiceCode)! : undefined;
    name = item.name ? `${item.name}:${name}` : name;
    return (
      <DependencyTreeNode offsetBottom={10} style={{ height: showAdditionalLine ? 54 : 30 }}>
        <div className={styles.node}>
          <div className={styles.top} style={{ height: showAdditionalLine ? undefined : 30 }}>
            <span className={styles.node_left}>
              {!!item.children?.length && <Icon type="folder-o" className={styles.node_left_icon} />}
              <span className={styles.node_text}>
                {item.versionAlias && (
                  <span className={styles.node_left_alias}>
                    {item.versionAlias}
                    &nbsp;
                  </span>
                )}
                {appService ? <span>{`${appService.name}（${appService.code}）`}</span> : <span>{item.name}</span>}

              </span>
              {item.tagName && (
                <span className={styles.tag}>
                  {item.tagName}
                </span>
              )}
            </span>

            {level === 0 ? (
              <span>
                {item.type === 'tag' && (
                <Button
                  icon="mode_edit"
                  className={styles.node_btn}
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
                )}
                <Button
                  icon="delete_forever"
                  className={styles.node_btn}
                  onClick={(e) => {
                    e.stopPropagation();
                    handleDelete(item);
                  }}
                />
              </span>
            ) : null}
          </div>
          {showAdditionalLine && (
            <div className={styles.bottom}>
              {item.artifactId && <span className={styles.node_text}>{`artifactID：${item.artifactId}`}</span>}
              {item.groupId && <span className={styles.node_text}>{`groupID：${item.groupId}`}</span>}
              {item.version && <span className={styles.node_text}>{`versionID：${item.version}`}</span>}
            </div>
          )}

        </div>
      </DependencyTreeNode>
    );
  }
  async function handleImportPom(pomData: any) {
    await publishVersionApi.createBatch(detailData.id, pomData);
    store.loadData();
    return true;
  }
  async function handleLinkTag(linkData: any) {
    console.log('linkData', linkData);
    publishVersionApi.dependencyTreeAddTag(detailData.id, linkData).then(() => {
      store.loadData();
    });
    return false;
  }
  return (
    <PublishVersionSection
      className={styles.wrap}
      title={(
        <div className={styles.title}>
          <span>关联版本</span>
          <div className={styles.operation}>
            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
              icon="local_offer"
              onClick={() => {
                openLinkAppServiceTagModal({ publishVersionId: detailData.id, handleOk: handleLinkTag });
              }}
            >
              关联tag
            </Button>
            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
              icon="version"
              onClick={() => {
                openLinkPublishVersionModal({ publishVersionId: detailData.id, handleOk: handleLinkPublishVersion });
              }}
            >
              关联发布版本
            </Button>

            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
              //   icon="mode_edit"
              onClick={() => {
                openImportPomModal({ handleOk: handleImportPom, versionId: detailData.id });
              }}
            >
              <span style={{ display: 'inline-flex', alignItems: 'center' }}>
                <CustomIcon type="icon-pom" width={18} height={18} />
                导入POM依赖
              </span>

            </Button>

          </div>
        </div>
      )}
    >

      {dependencyList.map((i) => <DependencyTreeBase data={[i]} renderNode={renderTreeNode} />)}
    </PublishVersionSection>
  );
}
export default observer(PublishVersionLinkVersion);
