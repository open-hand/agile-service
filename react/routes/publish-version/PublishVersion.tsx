import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb, useTheme,
} from '@choerodon/boot';
import {
  Button, Tooltip,
} from 'choerodon-ui/pro';
import { HeaderButtons } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import { omit } from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import SideNav from '@/components/side-nav';
import Empty from '@/components/Empty';
import { Loading } from '@/components';
import Provider, { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-edit-publish-version';
import openExportPublishVersionModal from './components/export';
import PublishVersionList from './components/list';
import Container from './Container';
import './PublishVersion.less';
import empty from './empty.svg';
import { IPublishVersionProps } from '.';

const TooltipButton: React.FC<{ title?: string, buttonIcon: string, buttonDisabled?: boolean, clickEvent?: () => void } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, buttonIcon, buttonDisabled = false, clickEvent, ...otherProps
}) => {
  if (title && buttonDisabled) {
    return <Tooltip title={title}><Button disabled={buttonDisabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return (
    <Button
      {...otherProps}
      onClick={clickEvent}
      icon={buttonIcon}
      disabled={buttonDisabled}
    >
      {children}
    </Button>
  );
};

function PublishVersion() {
  const context = usePublishVersionContext();
  const {
    prefixCls, tableDataSet, store, pageHeader, pageContentEmpty, renderPage,
  } = context;
  const [theme] = useTheme();

  function handleCreate(data: any) {
    store.create(data).then((res) => {
      // 初次进入 无数据 创建后查询 并选中
      typeof (res) === 'boolean' && !res && tableDataSet.query().then(() => {
        tableDataSet.select(tableDataSet.records[0]);
        store.select(tableDataSet.records[0].toData());
      });
    });
  }

  function render() {
    if (tableDataSet.status === 'loading' && !tableDataSet.getState('searchMode') && tableDataSet.length === 0) {
      return <Loading loading />;
    }
    return tableDataSet.getState('searchMode') || tableDataSet.length > 0 ? (
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

    ) : (pageContentEmpty
      || (
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
    );
  }
  const renderPageHeader = () => (
    <Header>
      {pageHeader || (
        <HeaderButtons
          items={[
            {
              display: true,
              element: (
                <TooltipButton
                  buttonIcon="playlist_add"
                  title="无相应权限创建发布版本"
                  clickEvent={() => {
                    openCreatePublishVersionModal({ handleOk: handleCreate });
                  }}
                >
                  创建发布版本
                </TooltipButton>
              ),
            },
            {
              name: '导出版本',
              icon: 'unarchive-o',
              handler: () => {
                openExportPublishVersionModal(store.getCurrentData.id);
              },
              display: !!store.getCurrentData.id,
            },
          ]}
        />
      )}

    </Header>
  );
  const renderContent = () => (
    <Content
      style={{
        ...theme === 'theme4' ? {
          marginLeft: 0,
          marginRight: 0,
          paddingLeft: 0,
          paddingRight: 0,
          paddingBottom: 0,
        } : {
          padding: 0, paddingTop: 4,
        },
      }}
      className={`${prefixCls}-content`}
    >
      {render()}

    </Content>
  );
  return renderPage ? renderPage(renderPageHeader, render) : (
    <Page className={prefixCls}>
      {renderPageHeader()}
      <Breadcrumb />
      {renderContent()}
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
