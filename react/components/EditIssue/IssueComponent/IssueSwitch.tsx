import React, {
  useState, useEffect, useRef, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import { Issue } from '@/common/types';
import IssueList from '../Component/IssueList';
import styles from './IssueSwitch.less';

function useClickOut(onClickOut: Function) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    // @ts-ignore
    if (ref.current && !ref.current.contains(e.target)) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick);
    return () => {
      document.removeEventListener('click', handleClick);
    };
  }, [handleClick]);
  return ref;
}

interface Props {
  issue: Issue,
  reloadIssue: Function,
}

const IssueSwitch: React.FC<Props> = ({ issue, reloadIssue }) => {
  const [visible, setVisible] = useState<boolean>(false);
  const {
    issueId, typeCode, relateIssueId, parentIssueId, sameParentBugVOList, sameParentIssueVOList,
  } = issue;
  const parentId = parentIssueId || relateIssueId;
  const sameParentList = ((typeCode === 'sub_task' ? sameParentIssueVOList : sameParentBugVOList) || []).filter((item) => item.issueId !== issueId);
  const renderIssueList = (item:Issue, i: number) => (
    <IssueList
      showStar
      style={{
        borderBottom: sameParentList.length > 1 ? '1px solid var(--divider)' : '',
      }}
      showAssignee={false}
      key={item.issueId}
      issue={{
        ...item,
        typeCode: item.typeCode || 'sub_task',
      }}
      i={i}
      onOpen={() => {
        if (reloadIssue) {
          reloadIssue(item.issueId);
        }
      }}
    />
  );

  const renderOverlay = () => (
    <div className={styles.issue_switch_overlay}>
      {
        // @ts-ignore
        sameParentList.map((item, i, arr) => renderIssueList(item, i === 0 ? arr.length : i))
      }
    </div>
  );

  const handleClickIcon = () => {
    setVisible(!visible);
  };

  useEffect(() => {
    setVisible(false);
  }, [issue.issueId]);

  const handleClickOut = useCallback(() => {
    setVisible(false);
  }, []);
  const ref = useClickOut(handleClickOut);
  return (
    <>
      {
        // @ts-ignore
        ['sub_task', 'bug'].includes(typeCode) && Boolean(parentId) && sameParentList.length > 0 && (
          // @ts-ignore
          <span className={styles.issue_switch} ref={ref}>
            <span className={styles.issue_switch_clickTarget}>
              <Icon
                type={!visible ? 'expand_more' : 'expand_less'}
                onClick={handleClickIcon}
              />
            </span>
            {visible && renderOverlay()}
          </span>
        )
      }
    </>
  );
};

export default observer(IssueSwitch);
