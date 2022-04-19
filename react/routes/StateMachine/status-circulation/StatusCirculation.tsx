import React, { useEffect, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { Divider } from 'choerodon-ui';
import { find } from 'lodash';
import { getIsOrganization } from '@/utils/common';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import { IIssueType } from '@/common/types';
import openSelectExistStatus from '../components/select-exist-status';
import openCreateStatus from '../components/create-status';
import openSetDefaultStatus from '../components/set-default-status';
import openConfirmLeave from './components/confirm-leave';
import StatusCirculationTable from './components/status-circulation-table';
import { TabComponentProps } from '..';
import { useStatusCirculationContext } from './index';
import { useStateMachineContext } from '../context';
import IssueTypeTab from '../components/issue-type-tab';
import useFormatMessage from '@/hooks/useFormatMessage';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

const StatusCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const { store } = useStatusCirculationContext();
  const formatMessage = useFormatMessage('agile.stateMachine');
  const { data: issueTypes } = useIssueTypes();
  const {
    selectedType, setSelectedType, issueTypeInitedMap, readOnly, visibleIssueTypeCategory, noContainer,
  } = useStateMachineContext();
  const isOrganization = getIsOrganization();
  // const selectedTypeRef = useRef<string>(selectedType);
  // selectedTypeRef.current = selectedType;
  const refresh = useCallback(() => {
    if (selectedType && issueTypes?.find((item: IIssueType) => item.id === selectedType) && ((isOrganization && issueTypeInitedMap.get(selectedType)) || !isOrganization)) {
      store.getStatusList(selectedType);
    }
  }, [isOrganization, issueTypeInitedMap, issueTypes, selectedType, store]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  // useEffect(() => () => {
  //   if (store.hasAction) {
  //     openConfirmLeave({
  //       onOk: async () => {
  //         await store.batchUpdateStatusTransform(selectedTypeRef.current);
  //       },
  //     });
  //   }
  // }, []);

  const content = (
    <>
      {
        !readOnly && (
          <>
            <Header>
              <HeaderButtons items={[
                {
                  name: formatMessage({ id: 'flow.add.exist.state' }),
                  display: true,
                  handler: () => {
                    const issueType = find(issueTypes, (item) => item.id === selectedType);
                    openSelectExistStatus({
                      applyType: issueType?.typeCode && WATERFALL_TYPE_CODES.includes(issueType?.typeCode) ? 'waterfall' : undefined,
                      statusList: store.statusList,
                      issueTypeId: selectedType,
                      onSubmit: () => {
                        refresh();
                      },
                    });
                  },
                  icon: 'playlist_add',
                },
                {
                  name: formatMessage({ id: 'flow.create.new.state' }),
                  display: true,
                  handler: () => {
                    openCreateStatus({
                      selectedIssueType: [selectedType],
                      onSubmit: () => {
                        refresh();
                      },
                    });
                  },
                  icon: 'playlist_add',
                }, {
                  name: formatMessage({ id: 'flow.init.state' }),
                  display: true,
                  handler: () => {
                    openSetDefaultStatus({
                      issueTypeId: selectedType,
                      statusList: toJS(store.statusList),
                      onSubmit: refresh,
                    });
                  },
                  icon: 'settings-o',
                },
              ]}
              />
            </Header>
            <Breadcrumb />
          </>
        )
      }

      {
        !isOrganization && (
          <Divider style={{ margin: 0 }} />
        )
      }
      <Content style={{ display: 'flex', flexDirection: 'column', paddingBottom: 0 }}>
        {tab}
        <div style={{ marginLeft: 1 }}>
          <IssueTypeTab
            selectedType={selectedType}
            setSelectedType={(newType) => {
              if (store.hasAction) {
                openConfirmLeave({
                  onOk: async () => {
                    store.clearActions();
                    setSelectedType(newType);
                  },
                });
              } else {
                setSelectedType(newType);
              }
            }}
            excludeTypes={isOrganization ? ['feature', 'issue_epic', 'issue_auto_test', 'issue_test', 'risk'] : []}
            brighter={readOnly}
            visibleIssueTypeCategory={visibleIssueTypeCategory}
          />
        </div>
        <div style={{ flex: 1, overflow: 'hidden', display: 'flex' }}>
          <StatusCirculationTable />
        </div>
      </Content>
    </>
  );
  return (
    <>
      {
        noContainer ? content : (
          <Page>
            {content}
          </Page>
        )
      }
    </>
  );
};

export default observer(StatusCirculation);
