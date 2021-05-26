import React, { memo, useMemo } from 'react';
import {
  Button, Modal, Icon,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  IPublishVersionTreeNode, publishVersionApi,
} from '@/api';
import CustomIcon from '@/components/custom-icon';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import classNames from 'classnames';
import DependencyTreeBase, { TreeWithLineNode as DependencyTreeNode } from '@/components/tree-with-line';
import { openImportPomModal } from '@/components/import-pom';
import { IPublishVersionLinkVersionNodeOperationProps } from '@/routes/publish-version';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';
import { openLinkAppServiceTagModal } from './LinkAppServiceTagModal';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';
import { openEditAppVersionModal, openEditLinkAppServiceModal } from './EditAppVersionModal';
import styles from './index.less';

export function useEditModeSectionConfig() {
  const { store } = usePublishVersionContext();

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
  async function handleImportPom(pomData: any) {
    await publishVersionApi.createBatch(detailData.id, pomData);
    store.loadData();
    return true;
  }
  async function handleLinkTag(linkData: any) {
    publishVersionApi.dependencyTreeAddTag(detailData.id, linkData).then(() => {
      store.loadData();
    });
    return true;
  }
  const TitleRight = memo(({ operation }: { operation: any }) => (
    <div className={styles.operation}>
      <Button
        icon="local_offer-o"
        funcType={'flat' as any}
        className={styles.operation_btn} // 线框
        onClick={() => {
          typeof (operation.onClickTag) === 'function' ? operation?.onClickTag()
            : openLinkAppServiceTagModal({ publishVersionId: detailData.id, handleOk: handleLinkTag });
        }}
      >
        关联tag
      </Button>
      <Button
        icon="versionline"
        className={styles.operation_btn}
        funcType={'flat' as any}
        onClick={() => {
          typeof (operation.onClickVersion) === 'function' ? operation?.onClickVersion()
            : openLinkPublishVersionModal({ publishVersionId: detailData.id, handleOk: handleLinkPublishVersion });
        }}
      >
        关联发布版本
      </Button>

      <Button
        funcType={'flat' as any}
        className={styles.operation_btn}
        onClick={() => {
          typeof (operation.onClickPom) === 'function' ? operation?.onClickPom()
            : openImportPomModal({ handleOk: handleImportPom, versionId: detailData.id });
        }}
      >
        <span style={{ display: 'inline-flex', alignItems: 'center' }}>
          <CustomIcon type="archive-o" width={18} height={18} />
          导入POM依赖
        </span>

      </Button>

    </div>
  ));
  return {
    className: styles.wrap,
    bodyClassName: styles.wrap_body,
    titleClassName: styles.title,
    title: '关联版本',
    titleRight: <TitleRight operation={{}} />,
  };
}
function PublishVersionLinkVersion({ sectionProps, nodeOperationProps }: { sectionProps?: any, nodeOperationProps: IPublishVersionLinkVersionNodeOperationProps | undefined }) {
  const { store, preview } = usePublishVersionContext();
  const detailData = store.getCurrentData;
  const dependencyList = store.getDependencyList;
  console.log('dependencyList', store.getDependencyList);
  function handleDelete(v: IPublishVersionTreeNode) {
    let versionName = v.versionAlias || v.name;
    if (v.type === 'tag') {
      const appService = v.appServiceCode ? store.findAppServiceByCode(v.appServiceCode, v)! : undefined;
      versionName = appService ? `${appService.name}（${appService.code}）` : versionName;
    }

    Modal.confirm({
      title: '删除关联版本',
      children: (
        <div>
          <span>{`您确定要删除关联的版本【${versionName}】？`}</span>
          {/* <SelectBox mode={'box' as any} defaultValue="only" onChange={(value: any) => { delConfigRef.current = value; }}>
            <SelectBox.Option value="only">仅删除关联关系</SelectBox.Option>
            <SelectBox.Option value="all">删除关联关系及应用版本</SelectBox.Option>
          </SelectBox> */}
        </div>),
      onOk: async () => {
        if (typeof (nodeOperationProps?.onDelete) === 'function') {
          await nodeOperationProps.onDelete(v);
        } else {
          await (v.type === 'tag' ? publishVersionApi.dependencyTreeDelTag(detailData.id, [v as any])
            : publishVersionApi.dependencyTreeDel({
              id: detailData.id,
              type: 'publish',
              children: [
                { id: v.id, type: v.type },
              ],
            }));
        }
        store.loadData();
      },
    });
  }
  function renderTreeNode(item: IPublishVersionTreeNode, level: number) {
    const showAdditionalLine = !!(item.groupId || item.artifactId || item.version);
    const appService = item.appServiceCode ? store.findAppServiceByCode(item.appServiceCode, item)! : undefined;
    const alias = item.versionAlias || item.tagAlias;
    const nodeLeft = (
      <span className={styles.node_left}>
        {!!item.children?.length && <Icon type="folder-o" className={styles.node_left_icon} />}
        <span className={styles.node_text}>
          {alias && (
            <span className={styles.node_left_alias}>
              {alias}
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
    );

    return (
      <DependencyTreeNode offsetBottom={showAdditionalLine ? 10 : 0} style={{ height: showAdditionalLine ? 54 : 30 }}>
        <div className={styles.node} style={{ marginLeft: item.children?.length ? 4 : 0 }}>
          <div className={styles.top} style={{ height: showAdditionalLine ? undefined : 30 }}>
            {nodeOperationProps?.renderLeftNode ? nodeOperationProps.renderLeftNode(item, nodeLeft) : nodeLeft}
            {level === 0 && !preview ? (
              <span className={styles.node_operation}>
                {(typeof (nodeOperationProps?.isShowEdit) === 'function' ? nodeOperationProps.isShowEdit(item) : nodeOperationProps?.isShowEdit) && (
                  <Button
                    icon="edit-o"
                    className={styles.node_btn}
                    style={{ width: '.26rem', height: '.26rem' }}
                    onClick={(e) => {
                      e.stopPropagation();
                      openEditAppVersionModal({
                        data: item as any,
                        handleOk: async (newData) => {
                          typeof (nodeOperationProps?.onEdit) === 'function' ? await nodeOperationProps?.onEdit(newData, item)
                            : await publishVersionApi.updateTreeTagAlias(item.id, store.getCurrentData.id, item.objectVersionNumber!, newData.tagAlias);
                          store.loadData();
                          return true;
                        },
                      });
                    }}
                  />
                )}
                {(typeof (nodeOperationProps?.isShowDel) === 'function' ? nodeOperationProps?.isShowDel(item) : nodeOperationProps?.isShowDel) && (
                <Button
                  icon="delete_sweep-o"
                  style={{ width: '.26rem', height: '.26rem' }}
                  className={styles.node_btn}
                  onClick={(e) => {
                    e.stopPropagation();
                    handleDelete(item);
                  }}
                />
                )}
              </span>
            ) : null}
          </div>
          {showAdditionalLine && (
            <div className={styles.bottom}>
              {item.artifactId && <span className={styles.node_text}>{`Artifact：${item.artifactId}`}</span>}
              {item.groupId && <span className={styles.node_text}>{`Group：${item.groupId}`}</span>}
              {item.version && <span className={styles.node_text}>{`Version：${item.version}`}</span>}
            </div>
          )}

        </div>
      </DependencyTreeNode>
    );
  }
  return (
    <PublishVersionSection {...sectionProps}>
      {dependencyList.map((i) => <DependencyTreeBase data={[i]} renderNode={renderTreeNode} />)}
    </PublishVersionSection>
  );
}
PublishVersionLinkVersion.defaultProps = {
  sectionProps: undefined,
};
export default observer(PublishVersionLinkVersion);
