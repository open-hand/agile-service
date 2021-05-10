import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-publish-version';
import { openPublishVersionDetail } from './components/publish-version-detail';
import openExportPublishVersionModal from './components/export';
import PublishVersionList from './components/list';
import PublishVersionHeader from './components/Header';
import PublishVersionBody from './components/Body';
import Container from './Container';
import './PublishVersion.less';

const { Column } = Table;
const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};

function PublishVersion() {
  const { prefixCls, tableDataSet } = usePublishVersionContext();
  function handleClickMenu(key: string, record: Record) {
    switch (key) {
      case 'del':
        Modal.confirm({
          title: '删除发布版本',
          children: (
            <div>
              <span>{`您确定要删除发布版本【${record.get('versionAlias') || record.get('version')}】？`}</span>
            </div>),
          onOk: () => {
            publishVersionApi.delete(record.get('id')).then(() => {
              tableDataSet.query();
            });
          },
        });
        break;
      case 'version_planning':
      case 'released': {
        publishVersionApi.update(record.get('id'), {
          ...record.toData(),
          statusCode: key,
        }, key).then(() => tableDataSet.query(tableDataSet.currentPage));
        break;
      }
      default:
        break;
    }
  }
  function handleRefresh() {
    tableDataSet.query();
  }

  return (
    <Page>
      <Header>
        <TooltipButton
          icon="playlist_add"
          title="无相应权限创建发布版本"
          onClick={() => openCreatePublishVersionModal({ handleOk: handleRefresh })}
        >
          创建发布版本
        </TooltipButton>
        <Button icon="unarchive" onClick={openExportPublishVersionModal}>导出版本</Button>
      </Header>
      <Breadcrumb />
      <Content
        className={`${prefixCls}-content`}

      >
        <SideNav>
          <SideNav.Panel
            key="version"
            tabKey="version"
            title="版本列表"
            active={false}

          >
            <PublishVersionList />
          </SideNav.Panel>
        </SideNav>
        <Container />
      </Content>
    </Page>
  );
}
export default observer(PublishVersion);
