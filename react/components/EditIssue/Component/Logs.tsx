/*eslint-disable */
import React, { Component } from 'react';
import { Icon, Popover, Tooltip } from 'choerodon-ui';
import TimeAgo from 'timeago-react';
import _ from 'lodash';
import UserHead from '../../UserHead';
import './Logs.less';
import { ILog } from '@/common/types';

interface LogProps {
  log: ILog,
}

let fieldsMap = new Map();

fieldsMap = new Map([
['Sprint', {
  name: '冲刺',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          更新
          <span className="c7n-Log-field">【冲刺】</span>
          为
          <span className="c7n-Log-value">{`【${newString}】`}</span>
        </span>
      )
    }
  },
}],
[
  'status', {
    name: '状态'
  },
],
['Story Points', {
  name: '故事点',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          将
          <span className="c7n-Log-field">【故事点】</span>
          <span>由</span>
          <span className="c7n-Log-value">{`【未预估】`}</span>
          改变为
          <span className="c7n-Log-value">{`【${Number(newString)}】`}</span>
        </span>
      );
    },
  },
  update: {
    transform: ({oldString, newString}: {oldString?: string, newString?: string}) => {
      if(oldString) {
        return Number(oldString);
      } else if(newString) {
        return Number(newString);
      }
      return '';
    }
  },
  delete: {
    render: (log: ILog) => {
      const { oldString } = log;
      return (
        <span>
          将
          <span className="c7n-Log-field">【故事点】</span>
          <span>由</span>
          <span className="c7n-Log-value">{`【${Number(oldString)}】`}</span>
          改变为
          <span className="c7n-Log-value">【未预估】</span>
        </span>
      );
    },
  },
}],
['timeestimate', {
  name: '剩余预估时间',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          将
          <span className="c7n-Log-field">【剩余预估时间】</span>
          <span>由</span>
          <span className="c7n-Log-value">{`【未预估】`}</span>
          改变为
          <span className="c7n-Log-value">{`【${Number(newString)}】`}</span>
        </span>
      );
    },
  },
  update: {
    transform: ({oldString, newString}: {oldString?: string, newString?: string}) => {
      if(oldString) {
        return Number(oldString);
      } else if(newString) {
        return Number(newString);
      }
      return '';
    }
  },
  delete: {
    render: (log: ILog) => {
      const { oldString } = log;
      return (
        <span>
          将
          <span className="c7n-Log-field">【剩余预估时间】</span>
          <span>由</span>
          <span className="c7n-Log-value">{`【${Number(oldString)}】`}</span>
          改变为
          <span className="c7n-Log-value">【未预估】</span>
        </span>
      );
    },
  },
}],
['summary', {
  name: '问题概要',
}],
['Attachment', {
  name: '附件',
  create: {
    operation: '上传',
    transform: ({ newString }: { newString: string}) => newString.split('@')[1],
  },
  delete: {
    operation: '删除',
    transform: ({ oldString }: {oldString: string }) => oldString.split('@')[1],
  },
}],
['Comment', {
  name: '评论',
  create: {
    hidden: true,
  },
  update: {
    hidden: true,
  },
  delete: {
    operation: '删除',
    hidden: true,
  },
}],
['description', {
  name: '描述',
  create: {
    hidden: true,
  },
  update: {
    hidden: true,
  },
  delete: {
    hidden: true,
  },
}],
['Epic Name', {
  name: '史诗名称',
}],
['priority', {
  name: '优先级',
}],
['labels', {
  name: '标签',
  create: {
    condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.trim().split(' '), oldString && oldString.trim().split(' ')).length > 0,
    transform: ({ newString, oldString }: {newString: string, oldString: string}) => {
      return _.difference(newString && newString.trim().split(' '), oldString && oldString.trim().split(' ')).join(',');
    },
  },
  update: {
    dontJudge: true,
  },
  delete: {
    condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.trim().split(' '), newString && newString.trim().split(' ')).length > 0,
    transform: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.trim().split(' '), newString && newString.trim().split(' ')).join(','),
  },
}],
['Epic Link', {
  name: '史诗',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          更新
          <span className="c7n-Log-field">【史诗】</span>
          为
          <span className="c7n-Log-value">{`【${newString}】`}</span>
        </span>
      )
    }
  },
}],
['assignee', {
  name: '经办人',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          更新
          <span className="c7n-Log-field">【经办人】</span>
          为
          <span className="c7n-Log-value">{`【${newString}】`}</span>
        </span>
      )
    }
  },
}],
['reporter', {
  name: '报告人',
  create: {
    render: (log: ILog) => {
      const { newString } = log;
      return (
        <span>
          更新
          <span className="c7n-Log-field">【报告人】</span>
          为
          <span className="c7n-Log-value">{`【${newString}】`}</span>
        </span>
      )
    }
  },
}],
['Component', {
  name: '模块',
}],
[
  'Version', {
    name: '影响的版本',
  }
],
[
  'Fix Version', {
    name: '修复的版本',
  }
],
[
  'timespent', {
    name: '花费时间',
    create: {
      render: () => (<span>
        更新
        <span className="c7n-Log-field">【花费时间】</span>
      </span>)
    },
    update: {
      render: () => (<span>
        更新
        <span className="c7n-Log-field">【花费时间】</span>
      </span>)
    },
    delete: {
      dontJudge: true,
    },
  }
],
[
  'WorklogId', {
    name: '工作日志',
    create: {
      hidden: true,
    },
    update: {
      dontJudge: true,
    },
    delete: {
      hidden: true,
    },
  }
],
[
  'Rank', {
    name: '排序',
    create: {
      render: () => (<span>
        更新
        <span className="c7n-Log-field">【排序】</span>
      </span>)
    },
    update: {
      render: () => (<span>
        更新
        <span className="c7n-Log-field">【排序】</span>
      </span>)
    },
    delete: {
      dontJudge: true,
    },
  }
],
[
  'issuetype', {
    name: '类型',
    create: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
], 
[
 'resolution', {
   name: '解决状态',
   create: {
    render: () => (<span>
      更新
      <span className="c7n-Log-field">【解决状态】</span>
    </span>)
  },
  update: {
    render: () => (<span>
      更新
      <span className="c7n-Log-field">【解决状态】</span>
    </span>)
  },
  delete: {
    render: () => (<span>
      更新
      <span className="c7n-Log-field">【解决状态】</span>
    </span>)
  },
 } 
],
[
  'Knowledge Relation', {
    name: '知识文档',
    update: {
      dontJudge: true,
    },
  }
],
[
  'Backlog Link', {
    name: '需求',
    create: {
      condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.split(','), oldString && oldString.split(',')).length > 0,
      transform: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.split(','), oldString && oldString.split(',')).join(','),
    },
    update: {
      dontJudge: true,
    },
    delete: {
      condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.split(','), newString && newString.split(',')).length > 0,
      transform: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.split(','), newString && newString.split(',')).join(','),
    },
  }
],
[
  'Feature Link', {
    name: '特性',
  }
],
[
  'Pi', {
    name: 'PI',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            更新
            <span className="c7n-Log-field">【PI】</span>
            为
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </span>
        )
      }
    },
  }
],
[
  'SubTeam', {
    name: '负责的子团队',
    create: {
      condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.split(','), oldString && oldString.split(',')).length > 0,
      transform: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.split(','), oldString && oldString.split(',')).join(','),
    },
    update: {
      condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(newString && newString.split(','), oldString && oldString.split(',')).length === 0,
    },
    delete: {
      condition: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.split(','), newString && newString.split(',')).length > 0,
      transform: ({ newString, oldString }: {newString: string, oldString: string}) => _.difference(oldString && oldString.split(','), newString && newString.split(',')).join(','),
    },
  }
],
[
  'issue_epic', {
    name: '史诗',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【史诗】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'story', {
    name: '故事',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【故事】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'bug', {
    name: '缺陷',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【缺陷】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'task', {
    name: '任务',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【任务】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'sub_task', {
    name: '子任务',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【子任务】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'feature', {
    name: '特性',
    create: {
      render: (log: ILog) => {
        return (
          <span>
            创建
            <span className="c7n-Log-value">【特性】</span>
          </span>
        )
      }
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    }
  }
],
[
  'Epic Child', {
    name: '史诗关联任务',
    update: {
      dontJudge: true,
    },
  }
],
[
  'Feature Child', {
    name: '特性关联任务',
    update: {
      dontJudge: true,
    },
  }
],
[
  'autoUpdate', {
    create: {
      dontJudge: true,
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    },
    customRender: (log: ILog) => {
      const { newStatus, trigger, autoRelutionUpdate} = log;
      return (
        <span>
           <span>
            变更了
            <span className="c7n-Log-value">{`【${trigger}】`}</span>
            ，使得当前问题自动流转到
            <span className="c7n-Log-value">{`【${newStatus}】`}</span>
            {
              autoRelutionUpdate && (
                <>
                  ，更新
                <span className="c7n-Log-value">【解决状态】</span>
                </>
              )
            }
          </span>
        </span>
      )
    }
  }
],
]);

const getFieldConfig = (log: ILog) => {
  const {field, fieldName} = log;
  if(fieldsMap.get(field)) {
    return fieldsMap.get(field);
  } else {
    return (
      {
        name: fieldName,
      }
    )
  }
};
const renderCreateDefault = (log: ILog) => {
const {
  field, newString, oldString,
} = log;
const fieldConfig = getFieldConfig(log);
return fieldConfig && (
  <>
    <span className="c7n-Log-operation">{fieldConfig.create?.operation || '添加'}</span>
    <span className="c7n-Log-field">{`【${fieldConfig.name}】`}</span>
    {
      !(fieldConfig.create && fieldConfig.create.hidden) && (
      <span className="c7n-Log-value">{`【${fieldConfig.create?.transform ? fieldConfig.create.transform({ newString, oldString }) : newString}】`}</span>
      )
   }
  </>
); 
};

const renderUpdateDefault = (log: ILog) => {
const {
  field, oldString, newString,
} = log;
const fieldConfig = getFieldConfig(log);
return fieldConfig && (
  <>
    {
      !(fieldConfig.update && fieldConfig.update.hidden) ? (
        <>
          <span>将</span>
          <span className="c7n-Log-field">{`【${fieldConfig.name}】`}</span>
          <span>由</span>
          <span className="c7n-Log-value">{`【${fieldConfig.update?.transform ? fieldConfig.update.transform({ oldString }) : oldString}】`}</span>
          <span>改变为</span>
          <span className="c7n-Log-value">{`【${fieldConfig.update?.transform ? fieldConfig.update.transform({ newString }) : newString}】`}</span>
        </>
      ) : (
        <>
          <span className="c7n-Log-operation">{fieldConfig.update?.operation || '更新'}</span>
          <span className="c7n-Log-field">{`【${fieldConfig.name}】`}</span>
        </>
      )
  }
  </>
);
};

const renderDeleteDefault = (log: ILog) => {
const {
  field, oldString, newString,
} = log;
const fieldConfig = getFieldConfig(log);
return fieldConfig && (
  <>
    <span className="c7n-Log-operation">{fieldConfig.delete?.operation || '移除'}</span>
    <span className="c7n-Log-field">{`【${fieldConfig.name}】`}</span>
    {
        !(fieldConfig.delete && fieldConfig.delete.hidden) && (
          <span className="c7n-Log-value">{`【${fieldConfig.delete?.transform ? fieldConfig.delete.transform({ newString, oldString }) : oldString}】`}</span>
        )
    }
  </>
);
};

const Log: React.FC<LogProps> = ({ log }) => {
const {
  field, oldValue, newValue, oldString, newString,
} = log;
const fieldConfig = getFieldConfig(log);
if (fieldConfig) {
  const createDontJudge = fieldConfig.create?.dontJudge;
  const updateDontJudge = fieldConfig.update?.dontJudge;
  const deleteDontJudge = fieldConfig.delete?.dontJudge;

  const createCondition = fieldConfig.create?.condition;
  const updateCondition = fieldConfig.update?.condition;
  const deleteCondition = fieldConfig.delete?.condition;

  const oldV = oldValue || oldString;
  const newV = newValue || newString;
  const renderLog = () => {
    
    const isCreate = Boolean((!oldV && String(oldV) !== '0') && (newV || String(newV) === '0'));
    const isUpdate = Boolean((oldV || String(oldV) === '0') && (newV || String(newV) === '0'));
    const isDelete = Boolean((oldV || String(oldV) === '0') && (!newV && String(newV) !== '0'));

    if (!createDontJudge && (createCondition?.({ newString, oldString }) || isCreate)) { // 新增
      return (fieldConfig.create?.render || renderCreateDefault)(log);
    } else if (!updateDontJudge && (updateCondition?.({ newString, oldString }) || isUpdate)) { // 修改
      return (fieldConfig.update?.render || renderUpdateDefault)(log);
    } else if (!deleteDontJudge && (deleteCondition?.({ newString, oldString }) || isDelete)) { // 删除
      return (fieldConfig.delete?.render || renderDeleteDefault)(log);
    } else {
      return fieldConfig?.customRender ?  fieldConfig?.customRender(log) : '';
    }
  };
    
  return (
    <>
      {renderLog()}
    </>
  );
}
return <></>;
};

interface Props {
  datalogs: ILog[],
  expand: boolean,
}
const Logs: React.FC<Props> = ({ datalogs, expand }) => {
  return (
    <div className="c7n-Logs">
    {
      (datalogs || []).map((log: ILog, i: number, arr: ILog[]) => ((i > 4 && expand) || i < 4) && (
      <div key={log.logId} className="c7n-Logs-log">
        <div
          className="c7n-Logs-log-user"
        >
          {
              i && log.lastUpdatedBy === arr[i - 1].lastUpdatedBy ? null : (
                <UserHead
                // @ts-ignore
                  user={{
                    id: log.lastUpdatedBy,
                    name: log.name,
                    loginName: log.loginName,
                    realName: log.realName,
                    avatar: log.imageUrl,
                  }}
                  hiddenText
                  type="datalog"
                />
              )
          }
        </div>
        <div className="c7n-Logs-log-right">
          <div className="c7n-Logs-log-logOperation">
            <span className="c7n-Logs-log-userName">{log.realName}</span>
            <Log log={log} />
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
}
export default Logs;
