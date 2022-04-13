import React, { useCallback, useContext, useMemo } from 'react';
import { Icon, Tooltip } from 'choerodon-ui';
import { withRouter } from 'react-router-dom';
import copy from 'copy-to-clipboard';
import { Choerodon } from '@choerodon/boot';
import { LINK_URL_TO } from '@/constants/LINK_URL';
import { issueApi } from '@/api';
import { linkUrl } from '@/utils/to';
import EditIssueContext from '../stores';
import IssueSwitch from './IssueSwitch';
import styles from './IssueNumber.less';
import { WATERFALL_TYPE_CODES } from '../../../constants/TYPE_CODE';

const IssueNumber = ({
  reloadIssue, typeCode, parentSummary, issue, disabled, otherProject, outside,
}) => {
  const { issueId, issueNum, applyType } = issue;
  const {
    isProgramIssue,
  } = useContext(EditIssueContext);
  const isWaterfallIssue = useMemo(() => WATERFALL_TYPE_CODES.includes(typeCode), [typeCode]);
  const handleClickIssueNum = useCallback(() => {
    if (disabled) {
      return false;
    }
    LINK_URL_TO.issueLinkTo(issueId, issueNum);
    return false;
  }, [disabled, issueId, issueNum]);

  const handleCopyLink = useCallback(async () => {
    let decryptIssueId = issueId;
    if (!/^[0-9]+$/.test(issueId)) {
      decryptIssueId = await issueApi.decrypt(issueId);
    }
    const queryData = {
      params: {
        paramIssueId: decryptIssueId, paramName: issueNum,
      },
    };
    if (applyType === 'waterfall') {
      copy(`${window.location.host}/#/${linkUrl('waterfall/wbs', queryData)}`);
    } else if (!isProgramIssue) {
      copy(`${window.location.host}/#/${linkUrl('agile/work-list/issue', queryData)}`);
    } else {
      copy(`${window.location.host}/#/${linkUrl('agile/feature', queryData)}`);
    }
    Choerodon.prompt('复制成功！');
  }, [isProgramIssue, issueId, issueNum]);

  return (
    <div style={{
      fontSize: 16, lineHeight: '28px', fontWeight: 500, whiteSpace: 'nowrap', display: 'flex', alignItems: 'center',
    }}
    >
      {
        ((['sub_task', 'bug'].includes(typeCode) && parentSummary) || ['feature', 'risk'].includes(typeCode) || isProgramIssue || isWaterfallIssue) ? (
          <span>
            {issueNum}
          </span>
        ) : (
          <a
            role="none"
            onClick={handleClickIssueNum}
            style={{ cursor: disabled ? 'auto' : 'pointer' }}
          >
            {issueNum}
          </a>
        )
      }
      {
        issueId && !otherProject && !outside && !isWaterfallIssue && typeCode !== 'risk' && (
          <Tooltip title="复制链接">
            <Icon type="link2" role="none" className={styles.copyLinkIcon} style={{ cursor: 'pointer' }} onClick={handleCopyLink} />
          </Tooltip>
        )
      }
      <IssueSwitch issue={issue} reloadIssue={reloadIssue} />
    </div>
  );
};

export default withRouter(IssueNumber);
