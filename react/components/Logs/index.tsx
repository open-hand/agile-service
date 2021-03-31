import React from 'react';
import { Tooltip } from 'choerodon-ui';
import TimeAgo from 'timeago-react';
import _ from 'lodash';
import { ILog } from '@/common/types';
import UserHead from '../UserHead';
import './Logs.less';
import Log from './Log';
import { UserUniqueTag } from '../tag/user-tag';

interface ILogConfig {
  operation?: string, // 操作名字
  render?: Function, // 定义这次操作的渲染
  transform?: Function, // 转换字段值
  condition?: Function, // 自定义判断条件
  hidden?: boolean, // 是否隐藏字段值
  dontJudge?: boolean, // 不进行判断
}

interface ILogTypeConfig {
  name?: string,
  create?: ILogConfig,
  update?: ILogConfig,
  delete?: ILogConfig,
  customRender?: Function,
}

export type IFieldMap = Map<string, ILogTypeConfig>

interface Props {
  datalogs: ILog[],
  expand: boolean,
  fieldsMap: IFieldMap,
}
const Logs: React.FC<Props> = ({ datalogs, expand, fieldsMap }) => (
  <div className="c7n-Logs">
    {
      (datalogs || []).map((log: ILog, i: number, arr: ILog[]) => ((i >= 4 && expand) || i < 5) && (
        <div key={log.logId} className="c7n-Logs-log">
          <div
            className="c7n-Logs-log-user"
          >
            {
              i && log.lastUpdatedBy === arr[i - 1].lastUpdatedBy ? null : (
                <UserUniqueTag
                  data={log.user ? {
                    id: log.user.id,
                    tooltip: log.user.name,
                    loginName: log.user.loginName,
                    realName: log.user.realName,
                    imageUrl: log.user.imageUrl,
                    ldap: log.user.ldap,
                  } : {
                    id: log.lastUpdatedBy,
                    tooltip: log.name,
                    loginName: log.loginName,
                    realName: log.realName,
                    imageUrl: log.imageUrl,
                  }}
                  showText={false}
                  avatarStyle={{
                    width: 40,
                    height: 40,
                    backgroundColor: '#c5cbe8',
                    color: '#6473c3',
                    borderRadius: 4,
                  }}
                />
              )
            }
          </div>
          <div className="c7n-Logs-log-right">
            <div className="c7n-Logs-log-logOperation">
              <span className="c7n-Logs-log-userName">{log.realName}</span>
              <Log log={log} fieldsMap={fieldsMap} />
            </div>
            <div className="c7n-Logs-log-lastUpdateDate">
              <Tooltip placement="top" title={log.lastUpdateDate || ''}>
                <TimeAgo
                  datetime={log.lastUpdateDate || ''}
                  locale="zh_CN"
                />
              </Tooltip>
            </div>
          </div>
        </div>
      ))
    }
  </div>
);
export default Logs;
