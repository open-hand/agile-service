import React, { createContext, useMemo } from 'react';
import PropTypes from 'prop-types';
import { observer } from 'mobx-react-lite';
import Animate from 'choerodon-ui/lib/animate';
import { DEFAULT, PROGRAM_ISSUE_IN_PROJECT, PROJECT_ISSUE_IN_PROGRAM } from './mode';
import IssueDetailStore from './EditIssueStore';

const IssueDetailContext = createContext();
const defaultProps = {
  mode: DEFAULT,
  applyType: 'agile',
};
const propTypes = {
  visible: PropTypes.bool,
  disabled: PropTypes.bool,
  mode: PropTypes.oneOf([
    DEFAULT,
    PROGRAM_ISSUE_IN_PROJECT,
    PROJECT_ISSUE_IN_PROGRAM,
  ]),
};
function IssueDetail({ mode, visible, disabled }) {
  const FieldVersionRef = {
    current: null,
  };
  const FieldFixVersionRef = {
    current: null,
  };
  const value = {
    mode,
    prefixCls: 'c7n-agile-IssueDetail',
    intlPrefix: 'agile.IssueDetail',
    store: useMemo(() => new IssueDetailStore(), []), // 防止update时创建多次store
    FieldVersionRef,
    FieldFixVersionRef,
    saveFieldVersionRef: (ref) => {
      FieldVersionRef.current = ref;
    },
    saveFieldFixVersionRef: (ref) => {
      FieldFixVersionRef.current = ref;
    },
  };

  return visible ? (
    <IssueDetailContext.Provider value={value}>
      {/* <Animate
        component="div"
        transitionAppear
        transitionName="slide-right"
        hiddenProp="hidden"
      > */}
      issuedetail
      {/* </Animate> */}
    </IssueDetailContext.Provider>
  ) : null;
}
IssueDetail.defaultProps = defaultProps;
IssueDetail.propTypes = propTypes;
IssueDetail.MODE = {
  DEFAULT,
  PROGRAM_ISSUE_IN_PROJECT,
  PROJECT_ISSUE_IN_PROGRAM,
};
export default observer(IssueDetail);
