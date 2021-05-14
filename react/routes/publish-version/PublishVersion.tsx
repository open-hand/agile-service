import React, { useEffect, useState } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Tooltip, Dropdown, Menu,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { omit } from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import SideNav from '@/components/side-nav';
import { linkUrl } from '@/utils/to';
import Empty from '@/components/Empty';
import { Loading } from '@/components';
import Provider, { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-edit-publish-version';
import openExportPublishVersionModal from './components/export';
import PublishVersionList from './components/list';
import Container from './Container';
import './PublishVersion.less';
import empty from './empty.svg';

const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};

function PublishVersion() {
  const { prefixCls, tableDataSet, store } = usePublishVersionContext();

  function handleCreate(data: any) {
    store.create(data).then(() => {
      tableDataSet.length === 0 && tableDataSet.query();
    });
  }
  useEffect(() => {
    console.log('tableDataSet', tableDataSet.length, tableDataSet.status);
  }, [tableDataSet.length, tableDataSet.status]);
  function handleClickMenu(key: string) {
    switch (key) {
      case 'excel':
        openExportPublishVersionModal(store.getCurrentData.id);
        break;
      case 'preview':
        window.open(`/#${linkUrl(`/agile/project-version/publish/preview/${store.getCurrentData.id}`, {
          type: 'project',
          params: {
            fullPage: 'true',
          },
        })}`);
        break;
      default:
        break;
    }
  }
  function renderMenu() {
    return (
      <Menu onClick={({ key }) => handleClickMenu(key)}>
        <Menu.Item key="excel">导出Excel</Menu.Item>
        <Menu.Item key="preview">在线预览</Menu.Item>

      </Menu>
    );
  }
  return (
    <Page className={prefixCls}>
      <Header>
        <TooltipButton
          icon="playlist_add"
          title="无相应权限创建发布版本"
          onClick={() => openCreatePublishVersionModal({ handleOk: handleCreate })}
        >
          创建发布版本
        </TooltipButton>
        {!!store.getCurrentData.id && (
          <Dropdown overlay={renderMenu()}>

            <Button icon="unarchive">导出版本</Button>
          </Dropdown>
        )}
      </Header>
      <Breadcrumb />
      <Content
        className={`${prefixCls}-content`}

      >
        {tableDataSet.status === 'loading' && tableDataSet.length === 0 ? <Loading loading /> : (
          <>
            {
              tableDataSet.length > 0 ? (
                <>
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
                </>

              ) : (
                <Empty
                  title="暂无可用发布版本"
                  description={(
                    <div>
                      {/* <span>为管理发布版本,创建发布版本</span> */}
                      <div>
                        <Button
                          funcType={'raised' as any}
                          style={{ marginTop: 10, fontSize: 14 }}
                          onClick={() => { openCreatePublishVersionModal({ handleOk: handleCreate }); }}
                        >
                          创建发布版本
                        </Button>
                      </div>
                    </div>
                  )}
                  pic={empty}
                />
              )
            }
          </>
        )}

      </Content>
    </Page>
  );
}
const ObserverPublishVersion = observer(PublishVersion);
export default function Index(props: any) {
  return (
    <Provider {...props}>
      <ObserverPublishVersion />
    </Provider>
  );
}
