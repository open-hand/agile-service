import React, {
  useMemo, useRef, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Animate } from 'choerodon-ui';
import CloseButton from './components/close-button';
import store from './store';
import IssueDetailContext from './context';
import styles from './index.less';


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
}

const IssueDetail: React.FC<Props> = ({ events }) => {
  const { visible } = store;
  const { selected } = store;
  useEffect(() => {
    store.initEvents(events);
  }, [events]);
  useEffect(() => {
    // eslint-disable-next-line no-console
    console.log('changed', selected);
  }, [selected]);
  return (
    <Animate
      component="div"
      transitionAppear
      transitionName="slide-right"
    >
      {
        visible ? (
          <IssueDetailContext.Provider value={{ store }}>
            <div
              className={styles.container}
            >
              issuedetail
              {selected}
              <CloseButton />
            </div>
          </IssueDetailContext.Provider>
        ) : null
      }
    </Animate>
  );
};

export default observer(IssueDetail);
