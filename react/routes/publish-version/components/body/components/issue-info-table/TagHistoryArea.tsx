import React, { useEffect, useState } from 'react';
import {
  Table,
} from 'choerodon-ui/pro';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import classnames from 'classnames';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { WSHandler } from '@choerodon/boot';
import { publishVersionApi } from '@/api';
import { StatusTag } from '@/components';
import { getProjectId } from '@/utils/common';
import styles from './TagHistoryArea.less';
import IssueTypeSwitch from '../switch';

const STATUS_MAP_TEXT = {
  done: { text: '完成', color: '#00BFA5' },
  doing: { text: '进行中', color: '#4D90FE' },
  failed: { text: '失败', color: '' },

};
interface Props {
    data: { lastUpdateDate: string, status: 'done' | 'doing' | 'failed' }
    onFinish?: () => void
}
function TagHistoryArea({ data, onFinish }: Props) {
  const status = STATUS_MAP_TEXT[data.status] || {};
  return (
    <WSHandler
      messageKey={`agile-preview-tag-compare-issues${getProjectId()}`}
      onMessage={(messageData: any) => {
        const newData = JSON.parse(messageData);
        const { action: newStatus } = newData || {};
        if (newStatus === 'done') {
          onFinish && onFinish();
        }
        console.log('Json...', newData);
      }}
    >
      <div className={styles.history}>
        <span className={styles.history_item}>
          最近更新时间
          <span>{`${data.lastUpdateDate}`}</span>
        </span>
        <span className={styles.history_item}>
          状态
          <span className={styles.history_tag} style={{ background: status.color }}>
            {status.text}
          </span>
        </span>
      </div>
    </WSHandler>
  );
}
TagHistoryArea.defaultProps = {
  onFinish: undefined,
};
export default TagHistoryArea;
