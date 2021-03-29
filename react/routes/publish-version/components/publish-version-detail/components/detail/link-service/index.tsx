import CustomIcon from '@/components/custom-icon';
import {
  Button, Icon, Modal, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import { Tree } from 'choerodon-ui';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import {
  IAppVersionData, IPublishVersionTreeNode, publishVersionApi, versionApi,
} from '@/api';
import { IAppVersionDataItem } from '@/components/release-detail/stores/store';
import { useReleaseDetailContext } from '../../../stores';
import { openImportPomModal } from '../../import-pom';
import Section from '../../section';
import { openLinkServiceModal } from '../../link-service-modal';
import './index.less';
import { openEditAppVersionModal, openCreateAppVersionModal } from './EditAppVersionModal';

const { TreeNode } = Tree;
const LinkService: React.FC = () => {
  const { disabled, prefixCls, store } = useReleaseDetailContext();
  const delConfigRef = useRef<string>('only');
  const data = store.getAppServiceList;
  const detailData = store.getCurrentData;
  function handleLinkAppService(linkData: any) {
    versionApi.linkAppVersions(detailData.versionId, linkData.version).then(() => {
      store.loadData();
    });
  }
  function handleDelete(v: IPublishVersionTreeNode) {
    Modal.confirm({
      title: '删除应用版本',
      children: (
        <div>
          <span>{`您确定要删除关联的应用版本【${v.versionAlias || v.version}】？`}</span>
          {/* <SelectBox mode={'box' as any} defaultValue="only" onChange={(value: any) => { delConfigRef.current = value; }}>
            <SelectBox.Option value="only">仅删除关联关系</SelectBox.Option>
            <SelectBox.Option value="all">删除关联关系及应用版本</SelectBox.Option>
          </SelectBox> */}
        </div>),
      onOk: () => {
        (publishVersionApi.dependencyTreeDel({
          id: detailData.id,
          children: [
            { id: v.id },
          ],
        })).then(() => {
          store.loadData();
        });
      },
    });
  }
  async function handleImportPom(pomData: any) {
    await publishVersionApi.createBatch(detailData.id, pomData);
    store.loadData();
    return true;
  }

  return (
    <Section
      title="依赖"
      buttons={
        !disabled ? (
          <div className={`${prefixCls}-link-service-operation`}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="创建应用版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                icon="playlist_add"
                onClick={() => {
                  openCreateAppVersionModal();
                }}
              />
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="导入pom文件">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                //   icon="mode_edit"
                onClick={() => {
                  openImportPomModal({ handleOk: handleImportPom, versionId: detailData.id });
                }}
              >
                <CustomIcon type="icon-pom" width={18} height={18} />
                {/* {svg} */}
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="关联应用版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                icon="local_offer"
                onClick={() => {
                  openLinkServiceModal({ versionId: detailData.id || detailData.versionId, handleOk: handleLinkAppService });
                }}
              />
            </Tooltip>
          </div>
        ) : ''
      }
      border
      contentClassName={`${prefixCls}-link-service`}
    >
      {data.map((item) => (
        <div role="none" className={`${prefixCls}-link-service-item`} onClick={() => {}}>
          <span className={`${prefixCls}-link-service-item-left`}>
            {item.type === 'service' ? <Icon type="local_offer" style={{ fontSize: 15 }} /> : <CustomIcon type="icon-pom" width={17} height={17} />}
            <span className={`${prefixCls}-link-service-item-left-text`}>{item.versionAlias || item.version}</span>
          </span>
          <Button
            icon="mode_edit"
            className={`${prefixCls}-link-service-item-btn`}
            onClick={(e) => {
              e.stopPropagation();
              openEditAppVersionModal({ data: item as any, handleOk: () => store.loadData() });
            }}
          />
          <Button
            icon="delete_forever"
            className={`${prefixCls}-link-service-item-btn`}
            onClick={(e) => {
              e.stopPropagation();
              handleDelete(item);
            }}
          />
        </div>
      ))}
    </Section>
  );
};
export default observer(LinkService);
