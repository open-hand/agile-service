import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Tooltip,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import SideNav from '@/components/side-nav';
import { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-publish-version';
import openExportPublishVersionModal from './components/export';
import PublishVersionList from './components/list';
import Container from './Container';
import './PublishVersion.less';

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
