import React from 'react';
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

import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { openLinkPublishVersionModal } from './LinkPublishVersionModal';
import { openLinkAppServiceTagModal } from './LinkAppServiceTagModal';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';

import styles from './index.less';
import DependencyTreeBase from '../dependency-tree-base';
import { openImportPomModal } from '../../../publish-version-detail/components/import-pom';

const { Column } = Table;
const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};

function PublishVersionLinkVersion() {
  const { prefixCls, store } = usePublishVersionContext();
  const dependencyList = store.getDependencyList[0]?.children || [];
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
  function renderTreeNode(item: IPublishVersionTreeNode, level: number) {
    let name = item.versionAlias || item.version;
    name = item.name ? `${item.name}:${name}` : name;
    return (
      <div role="none" className={styles.node}>
        <span className={styles.node_left}>
          <Icon type="folder-o" className={styles.node_left_icon} />
          <span className={styles.node_left_text}>{name}</span>
        </span>

        {level === 0 ? (
          <span>
            <Button
              icon="mode_edit"
              className={styles.node_btn}
              onClick={(e) => {
                e.stopPropagation();
                // openEditAppVersionModal({
                //   data: item as any,
                //   handleOk: async () => {
                //     store.loadData();
                //     return true;
                //   },
                // });
              }}
            />
            <Button
              icon="delete_forever"
              className={styles.node_btn}
              onClick={(e) => {
                e.stopPropagation();
                // handleDelete(item);
              }}
            />
          </span>
        ) : null}
      </div>
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
