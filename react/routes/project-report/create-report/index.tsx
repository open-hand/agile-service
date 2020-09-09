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
import ProjectReportContext from './context';
import ProjectReportStore from './store';

const CreateReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  return (
    <Page>
      <Header>
        <Button icon="playlist_add">添加报告内容</Button>
      </Header>
      <Breadcrumb />
      <Content>
        <ProjectReportContext.Provider value={{
          store,
        }}
        >
          <Dropdown
            trigger={['click' as Action]}
            overlay={(
              <Menu onClick={({ key }) => {
                openAddModal({
                  type: key as IReportContentType,
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
            <Button icon="add" color={'blue' as ButtonColor}>
              添加报告内容
            </Button>
            <BlockList />
          </Dropdown>
        </ProjectReportContext.Provider>
      </Content>
    </Page>
  );
};
export default CreateReport;
