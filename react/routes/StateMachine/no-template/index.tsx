import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Header, Page, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button, Tooltip } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import { EmptyPage } from '@choerodon/components';
import { getIsOrganization } from '@/utils/common';
import { statusTransformApi } from '@/api';
import NoData from '@/assets/image/NoData.svg';
import IssueTypeTab from '../components/issue-type-tab';
import { useStateMachineContext } from '../context';
import useFormatMessage from '@/hooks/useFormatMessage';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

const NoTemplate: React.FC<{ activeKey: string }> = ({ activeKey }) => {
  const {
    selectedType, setSelectedType, issueTypeInitedMap, setIssueTypeInitedMap, readOnly, visibleIssueTypeCategory,
  } = useStateMachineContext();
  const isOrganization = getIsOrganization();
  const formatMessage = useFormatMessage();
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
            {activeKey !== 'custom' && (
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
                    style={{ marginLeft: 16 }}
                  >
                    {formatMessage({ id: 'agile.stateMachine.flow.create.new.state' })}
                  </Button>
                </Tooltip>
                <Tooltip title="请先配置模板">
                  <Button
                    icon="settings-o"
                    disabled
                    color={'primary' as ButtonColor}
                    style={{ marginLeft: 16 }}
                  >
                    {formatMessage({ id: 'agile.stateMachine.flow.init.state' })}

                  </Button>
                </Tooltip>
              </Header>
            )}

          </>
        )
      }
      {!readOnly && <Breadcrumb />}
      <Content style={{ borderTop: 'none' }}>
        <IssueTypeTab
          selectedType={selectedType}
          setSelectedType={setSelectedType}
          excludeTypes={isOrganization ? ['feature', 'issue_epic', 'issue_auto_test', 'issue_test', 'risk', ...WATERFALL_TYPE_CODES] : []}
          brighter={readOnly}
          visibleIssueTypeCategory={visibleIssueTypeCategory}
        />
        <EmptyPage
          image={NoData}
          description={(
            <>
              {`当前工作项类型暂未配置状态机模板${readOnly ? '请到组织层状态机页面配置模板' : '点击按钮配置模板'}。`}
              {
                !readOnly && (
                  <>
                    <EmptyPage.Button
                      onClick={initTemplate}
                    >
                      【配置模板】
                    </EmptyPage.Button>
                  </>
                )
              }
            </>
          )}
        />
      </Content>
    </Page>
  );
};

export default observer(NoTemplate);
