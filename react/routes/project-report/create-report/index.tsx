import React, { useMemo } from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button, Dropdown, Menu } from 'choerodon-ui/pro';
import { IReportContentType } from '@/common/types';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import openAddModal from './components/add-modal';
import BlockList from './components/block-list';
import BaseInfo from './components/base-info';
import Operation from './components/operation';
import ProjectReportContext from './context';
import ProjectReportStore from './store';
import styles from './index.less';

const CreateReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  return (
    <Page>
      <Header>
        <Button icon="playlist_add">添加报告内容</Button>
      </Header>
      <Breadcrumb />
      <Content style={{ paddingBottom: 0 }}>
        <ProjectReportContext.Provider value={{
          store,
        }}
        >
          <div className={styles.header}>
            <div className={styles.tip} />
            <span className={styles.title}>基本信息</span>
          </div>
          <BaseInfo />
          <div>
            <div className={styles.header}>
              <div className={styles.tip} />
              <span className={styles.title}>报告内容</span>
              <Dropdown
                trigger={['click' as Action]}
                overlay={(
                  <Menu onClick={({ key }) => {
                    openAddModal({
                      type: key as IReportContentType,
                      store,
                    });
                  }}
                  >
                    <Menu.Item key="text">文本</Menu.Item>
                    <Menu.Item key="static_list">静态列表</Menu.Item>
                    <Menu.Item key="list">动态列表</Menu.Item>
                    <Menu.Item key="chart">图表</Menu.Item>
                  </Menu>
              )}
              >
                <Button icon="add" color={'blue' as ButtonColor} style={{ marginLeft: 10 }}>
                  添加报告内容
                </Button>
              </Dropdown>
            </div>
            <BlockList />
          </div>
          <Operation />
        </ProjectReportContext.Provider>
      </Content>
    </Page>
  );
};
export default CreateReport;
