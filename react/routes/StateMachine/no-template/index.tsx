import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Header, Page, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import Empty from '@/components/Empty';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { getIsOrganization } from '@/utils/common';
import { statusTransformApi } from '@/api';
import IssueTypeTab from '../components/issue-type-tab';
import { useStateMachineContext } from '../context';
// @ts-ignore
import empty from './empty.png';

const NoTemplate = () => {
  const {
    selectedType, setSelectedType, issueTypeInitedMap, setIssueTypeInitedMap, readOnly,
  } = useStateMachineContext();
  const isOrganization = getIsOrganization();

  const initTemplate = useCallback(() => {
    statusTransformApi.initTemplate(selectedType).then((res: any) => {
      if (!res.failed) {
        issueTypeInitedMap.set(selectedType, true);
        setIssueTypeInitedMap(issueTypeInitedMap);
      }
    });
  }, [issueTypeInitedMap, selectedType, setIssueTypeInitedMap]);

  return (
    <Page>
      {
        !readOnly && (
          <>
            <Header>
              <Tooltip title="请先配置模板">
                <Button
                  icon="playlist_add"
                  disabled
                >
                  添加已有状态
                </Button>
              </Tooltip>
              <Tooltip title="请先配置模板">
                <Button
                  icon="playlist_add"
                  disabled
                >
                  创建新的状态
                </Button>
              </Tooltip>
              <Tooltip title="请先配置模板">
                <Button
                  icon="settings-o"
                  disabled
                >
                  设置初始状态
                </Button>
              </Tooltip>
            </Header>
            <Breadcrumb />
          </>
        )
      }
      <Content style={{ borderTop: 'none' }}>
        <IssueTypeTab
          selectedType={selectedType}
          setSelectedType={setSelectedType}
          excludeTypes={isOrganization ? ['feature', 'issue_epic', 'issue_auto_test', 'issue_test'] : []}
          brighter={readOnly}
        />
        <Empty
          pic={empty}
          title="暂无模板"
          description={(
            <>
              {`当前问题类型暂未配置状态机模板${readOnly ? '请到组织层状态机页面配置模板' : '点击下方按钮配置模板'}。`}
              {
                !readOnly && (
                <>
                  <br />
                  <Button
                    style={{ fontSize: '14px', marginTop: 15 }}
                    color={'blue' as ButtonColor}
                    funcType={'raised' as FuncType}
                    onClick={initTemplate}
                  >
                    配置模板
                  </Button>
                </>
                )
              }
            </>
          )}
          imgStyle={{ width: 300 }}
        />
      </Content>
    </Page>
  );
};

export default observer(NoTemplate);
