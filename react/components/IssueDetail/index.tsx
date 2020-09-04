import React, {
  useMemo, useRef, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Animate } from 'choerodon-ui';
import CloseButton from './components/close-button';
import IssueNum from './components/issue-num';
import Container from './Container';
import store from './store';
import IssueDetailContext from './context';

export function useIssueDetailStore() {
  return store;
}
enum FieldKey {
  summary = 'summary'
}
interface Issue {
  issueId: number
}

export type Events = { [key: string]: Function };
interface Props {
  events: Events
  projectId?: number
}

const IssueDetail: React.FC<Props> = ({ events, projectId }) => {
  const { visible, selected } = store;
  useEffect(() => {
    store.initEvents(events);
  }, [events]);
  useEffect(() => {
    // eslint-disable-next-line no-console
    store.load();
  }, [selected]);
  return (
    <Animate
      component="div"
      transitionAppear
      transitionName="slide-right"
    >
      {
        visible ? (
          <IssueDetailContext.Provider value={{ store, projectId }}>
            <Container />
          </IssueDetailContext.Provider>
        ) : null
      }
    </Animate>
  );
};

export default observer(IssueDetail);
