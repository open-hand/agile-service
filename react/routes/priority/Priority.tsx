import React, { useCallback, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Content, Header, TabPage as Page, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
// @ts-ignore
import { FormattedMessage, useIntl } from 'react-intl';
import PriorityTable from './components/PriorityTable';
import openPriorityModal from './components/PriorityModal';

const Priority = () => {
  const intl = useIntl();
  const priorityTableRef = useRef<{query:() => void } | null>(null);

  const queryTable = useCallback(() => {
    if (priorityTableRef.current?.query) {
      priorityTableRef.current?.query();
    }
  }, []);
  return (
    <Page>
      <Header title={<FormattedMessage id="priority.title" />}>
        <HeaderButtons items={[{
          name: intl.formatMessage({ id: 'priority.create' }),
          display: true,
          icon: 'playlist_add',
          handler: () => openPriorityModal({
            onOk: queryTable,
          }),
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content>
        <PriorityTable priorityTableRef={priorityTableRef} />
      </Content>
    </Page>
  );
};

export default observer(Priority);
