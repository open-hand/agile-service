import CustomIcon from '@/components/custom-icon';
import {
  Button, Icon, Modal, Tooltip,
} from 'choerodon-ui/pro/lib';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { IAppVersionData, versionApi } from '@/api';
import { useReleaseDetailContext } from '../../../stores';
import { openImportPomModal } from '../../import-pom';
import Section from '../../section';
import { openLinkServiceModal } from '../../link-service-modal';
import './index.less';
import { openEditAppVersionModal } from './EditAppVersionModal';

const LinkService: React.FC = () => {
  const { disabled, prefixCls, store } = useReleaseDetailContext();
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
      children: `您确定要删除关联的应用版本【${v.versionAlias || v.version}】？`,
      onOk: () => versionApi.deleteLinkAppVersion(detailData.versionId, v.id!).then(() => {
        store.loadData();
      }),
    });
  }
  async function handleImportPom(pomData: any) {
    console.log('handleImportPom', pomData);
    await versionApi.createBranchAndLinkAppService(detailData.versionId, pomData);
    store.loadData();
    return true;
  }

  return (
    <Section
      title="关联应用服务版本"
      buttons={
        !disabled ? (
          <div className={`${prefixCls}-link-service-operation`}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="导入pom文件">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                //   icon="mode_edit"
                onClick={() => {
                  openImportPomModal({ handleOk: handleImportPom });
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
                  console.log('detailData', detailData);
                  openLinkServiceModal({ versionId: detailData.id || detailData.versionId, handleOk: handleLinkAppService });
                }}
              />
            </Tooltip>
          </div>
        ) : ''
      }
      contentClassName={`${prefixCls}-link-service`}
    >
      {data.map((item) => (
        <div role="none" className={`${prefixCls}-link-service-item`} onClick={() => openEditAppVersionModal({ data: item, handleOk: store.loadData })}>
          <span className={`${prefixCls}-link-service-item-left`}>
            {item.type === 'service' ? <Icon type="local_offer" style={{ fontSize: 15 }} /> : <CustomIcon type="icon-pom-multiColor" width={17} height={17} />}
            <span className={`${prefixCls}-link-service-item-left-text`}>{item.name}</span>
          </span>
          <Button icon="delete_forever" className={`${prefixCls}-link-service-item-btn`} onClick={() => handleDelete(item)} />
        </div>
      ))}
    </Section>
  );
};
export default observer(LinkService);
