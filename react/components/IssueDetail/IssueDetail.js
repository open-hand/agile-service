import React, {
  createContext, useMemo, useRef, useEffect,
} from 'react';
import PropTypes from 'prop-types';
import { observer } from 'mobx-react-lite';
import { throttle } from 'lodash';
import Animate from 'choerodon-ui/lib/animate';
import {
  DEFAULT, PROGRAM, PROGRAM_ISSUE_IN_PROJECT, PROJECT_ISSUE_IN_PROGRAM,
} from './mode';
import ResizeAble from '../ResizeAble';
import IssueDetailStore from './EditIssueStore';
import './IssueDetail.less';

const IssueDetailContext = createContext();
const defaultProps = {
  mode: DEFAULT,
  applyType: 'agile',
};
const propTypes = {
  visible: PropTypes.bool.isRequired,
  disabled: PropTypes.bool,
  mode: PropTypes.oneOf([
    DEFAULT,
    PROGRAM,
    PROGRAM_ISSUE_IN_PROJECT,
    PROJECT_ISSUE_IN_PROGRAM,
  ]),
  isFullScreen: PropTypes.bool,
  onChangeWidth: PropTypes.func,
};
const prefixCls = 'c7n-agile-IssueDetail';
function IssueDetail({
  visible, disabled, mode, isFullScreen, onChangeWidth,
}) {
  const container = useRef();
  const store = useMemo(() => new IssueDetailStore(), []);
  const { loadIssueDetail } = store;
  const value = {
    mode,
    prefixCls,
    intlPrefix: 'agile.IssueDetail',
    disabled,
    store,
  };

  useEffect(() => {
    loadIssueDetail(id);
  }, []);
  const handleResizeEnd = ({ width }) => {
    localStorage.setItem('agile.EditIssue.width', `${width}px`);
  };
  const setQuery = (width = container.current.clientWidth) => {
    if (width <= 600) {
      container.current.setAttribute('max-width', '600px');
    } else {
      container.current.removeAttribute('max-width');
    }
  };
  const handleResize = throttle(({ width }) => {
    setQuery(width);
    if (onChangeWidth) {
      onChangeWidth(width);// 设置宽度
    }
  }, 150);
  return visible ? (
    <IssueDetailContext.Provider value={value}>
      <Animate
        component="div"
        transitionAppear
        transitionName="slide-right"
      >
        <div
          className={prefixCls}
          style={{ top: isFullScreen ? 0 : 50 }}
        >
          <ResizeAble
            modes={['left']}
            size={{
              maxWidth: window.innerWidth * 0.6,
              minWidth: 440,
            }}
            defaultSize={{
              width: localStorage.getItem('agile.EditIssue.width') || 605,
              height: '100%',
            }}
            onResizeEnd={handleResizeEnd}
            onResize={handleResize}
          >
            <div className={`${prefixCls}-content`} ref={container}>
              issuedetail
            </div>
          </ResizeAble>
        </div>
      </Animate>
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
