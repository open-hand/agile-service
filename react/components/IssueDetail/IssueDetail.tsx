import React, {
  createContext, useMemo, useRef, useEffect,
} from 'react';
import { observer, useLocalStore } from 'mobx-react-lite';
import { Animate } from 'choerodon-ui';
import styles from './IssueDetail.less';

export const IssueDetailContext = createContext({});

interface Props {
  issueId: number | undefined
}
const IssueDetail: React.FC<Props> = ({ issueId }) => {
  const visible = Boolean(issueId);
  const store = useLocalStore(() => ({
    loading: false,
  }));

  return visible ? (
    <IssueDetailContext.Provider value={{ store }}>
      <Animate
        component="div"
        transitionAppear
        transitionName="slide-right"
      >
        <div
          className={styles.container}
        >
          issuedetail
        </div>
      </Animate>
    </IssueDetailContext.Provider>
  ) : null;
};

export default IssueDetail;
