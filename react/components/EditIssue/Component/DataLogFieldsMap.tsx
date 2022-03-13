import React from 'react';
import { difference } from 'lodash';
import moment from 'moment';
import { ILog } from '@/common/types';
import { MINUTE } from '@/constants/DATE_FORMAT';

function formatDate(value: string) {
  return value && moment(value).isValid() ? moment(value).format(MINUTE) : value;
}

function transformDate({ oldString, newString }: { oldString?: string, newString?: string }) {
  if (oldString) {
    return formatDate(oldString);
  } if (newString) {
    return formatDate(newString);
  }
  return '';
}

const fieldsMap = new Map([
  ['Sprint', {
    name: '冲刺',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【冲刺】</span>
            <span className="c7n-Log-operation">为</span>
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </span>
        );
      },
    },
  }],
  [
    'status', {
      name: '状态',
    },
  ],
  ['Story Points', {
    name: '故事点',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【故事点】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【未预估】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${Number(newString)}】`}</span>
          </span>
        );
      },
    },
    update: {
      transform: ({ oldString, newString }: { oldString?: string, newString?: string }) => {
        if (oldString) {
          return Number(oldString);
        } if (newString) {
          return Number(newString);
        }
        return '';
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【故事点】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${Number(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
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
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【剩余预估时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【未预估】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${Number(newString)}】`}</span>
          </span>
        );
      },
    },
    update: {
      transform: ({ oldString, newString }: { oldString?: string, newString?: string }) => {
        if (oldString) {
          return Number(oldString);
        } if (newString) {
          return Number(newString);
        }
        return '';
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【剩余预估时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${Number(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【未预估】</span>
          </span>
        );
      },
    },
  }],
  ['summary', {
    name: '工作项概要',
  }],
  ['Attachment', {
    name: '附件',
    create: {
      operation: '上传',
      transform: ({ newString }: { newString: string }) => newString.split('@')[1],
    },
    delete: {
      operation: '删除',
      transform: ({ oldString }: { oldString: string }) => oldString.split('@')[1],
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
      condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.trim().split(' '), oldString && oldString.trim().split(' ')).length > 0,
      transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.trim().split(' '), oldString && oldString.trim().split(' ')).join(','),
    },
    update: {
      dontJudge: true,
    },
    delete: {
      condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.trim().split(' '), newString && newString.trim().split(' ')).length > 0,
      transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.trim().split(' '), newString && newString.trim().split(' ')).join(','),
    },
  }],
  ['Epic Link', {
    name: '史诗',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【史诗】</span>
            <span className="c7n-Log-operation">为</span>
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </span>
        );
      },
    },
  }],
  ['assignee', {
    name: '经办人',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <>
            <span>
              <span className="c7n-Log-operation">更新</span>
              <span className="c7n-Log-field">【经办人】</span>
              <span className="c7n-Log-operation">为</span>
              <span className="c7n-Log-value">{`【${newString}】`}</span>
            </span>
          </>
        );
      },
    },
    update: {
      render: (log: ILog) => {
        const { newString, oldString } = log;
        return (
          <>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【经办人】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${oldString}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </>
        );
      },
    },
  }],
  ['reporter', {
    name: '报告人',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【报告人】</span>
            <span className="c7n-Log-operation">为</span>
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </span>
        );
      },
    },
  }],
  ['Component', {
    name: '模块',
  }],
  [
    'Version', {
      name: '影响的版本',
    },
  ],
  [
    'Fix Version', {
      name: '修复的版本',
    },
  ],
  [
    'timespent', {
      name: '耗费时间',
      create: {
        render: ({ newString, oldString }: { newString: string, oldString: string }) => (
          <span>
            <span className="c7n-Log-operation">登记</span>
            <span className="c7n-Log-field">【耗费时间】</span>
            <span className="c7n-Log-field">{`【${parseFloat(newString)}小时】`}</span>
          </span>
        ),
      },
      update: {
        render: ({ newString, oldString }: { newString: string, oldString: string }) => {
          const diff = Math.round(((parseFloat(newString) || 0) - (parseFloat(oldString) || 0)) * 10) / 10;
          return (
            <span>
              <span className="c7n-Log-operation">{diff > 0 ? '登记' : '移除'}</span>
              <span className="c7n-Log-field">【耗费时间】</span>
              <span className="c7n-Log-field">{`【${diff > 0 ? diff : -diff}小时】`}</span>
            </span>
          );
        },
      },
      delete: {
        render: ({ newString, oldString }: { newString: string, oldString: string }) => (
          <span>
            <span className="c7n-Log-operation">移除</span>
            <span className="c7n-Log-field">【耗费时间】</span>
            <span className="c7n-Log-field">{`【${parseFloat(oldString)}小时】`}</span>
          </span>
        ),
      },
    },
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
    },
  ],
  [
    'Rank', {
      name: '排序',
      create: {
        render: () => (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【排序】</span>
          </span>
        ),
      },
      update: {
        render: () => (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【排序】</span>
          </span>
        ),
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'issuetype', {
      name: '类型',
      create: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'resolution', {
      name: '解决状态',
      create: {
        render: () => (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【解决状态】</span>
          </span>
        ),
      },
      update: {
        render: () => (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【解决状态】</span>
          </span>
        ),
      },
      delete: {
        render: () => (
          <span>
            <span className="c7n-Log-operation">更新</span>
            <span className="c7n-Log-field">【解决状态】</span>
          </span>
        ),
      },
    },
  ],
  [
    'Knowledge Relation', {
      name: '知识文档',
      update: {
        dontJudge: true,
      },
    },
  ],
  [
    'Backlog Link', {
      name: '需求',
      create: {
        condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.split(','), oldString && oldString.split(',')).length > 0,
        transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.split(','), oldString && oldString.split(',')).join(','),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.split(','), newString && newString.split(',')).length > 0,
        transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.split(','), newString && newString.split(',')).join(','),
      },
    },
  ],
  [
    'Feature Link', {
      name: '特性',
    },
  ],
  [
    'Pi', {
      name: 'PI',
      create: {
        render: (log: ILog) => {
          const { newString } = log;
          return (
            <span>
              <span className="c7n-Log-operation">更新</span>
              <span className="c7n-Log-field">【PI】</span>
              <span className="c7n-Log-operation">为</span>
              <span className="c7n-Log-value">{`【${newString}】`}</span>
            </span>
          );
        },
      },
    },
  ],
  [
    'SubTeam', {
      name: '负责的子团队',
      create: {
        condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.split(','), oldString && oldString.split(',')).length > 0,
        transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(newString && newString.split(','), oldString && oldString.split(',')).join(','),
      },
      update: {
        condition: ({ newString, oldString }: { newString: string, oldString: string }) => {
          if (!newString || !oldString) {
            return false;
          }
          return difference(newString && newString.split(','), oldString && oldString.split(',')).length === 0;
        },
      },
      delete: {
        condition: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.split(','), newString && newString.split(',')).length > 0,
        transform: ({ newString, oldString }: { newString: string, oldString: string }) => difference(oldString && oldString.split(','), newString && newString.split(',')).join(','),
      },
    },
  ],
  ['createInitType', {
    name: '创建工作项',
    create: {
      render: (log: ILog) => (
        <span>
          <span className="c7n-Log-operation">创建</span>
          <span className="c7n-Log-value">{`【${log.newString}】`}</span>
        </span>
      ),
    },
    update: {
      dontJudge: true,
    },
    delete: {
      dontJudge: true,
    },
  }],
  [
    'issue_epic', {
      name: '史诗',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【史诗】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'story', {
      name: '故事',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【故事】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'bug', {
      name: '缺陷',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【缺陷】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'task', {
      name: '任务',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【任务】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'sub_task', {
      name: '子任务',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【子任务】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'feature', {
      name: '特性',
      create: {
        render: (log: ILog) => (
          <span>
            <span className="c7n-Log-operation">创建</span>
            <span className="c7n-Log-value">【特性】</span>
          </span>
        ),
      },
      update: {
        dontJudge: true,
      },
      delete: {
        dontJudge: true,
      },
    },
  ],
  [
    'Epic Child', {
      name: '史诗关联任务',
      update: {
        dontJudge: true,
      },
    },
  ],
  [
    'Feature Child', {
      name: '特性关联任务',
      update: {
        dontJudge: true,
      },
    },
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
        const {
          newStatus, trigger, removeResolution, resolutionChanged,
        } = log;
        return (
          <span>
            <span>
              <span className="c7n-Log-operation">变更了</span>
              <span className="c7n-Log-value">{`【${trigger}】`}</span>
              <span className="c7n-Log-operation">，使得当前工作项自动流转到</span>
              <span className="c7n-Log-value">{`【${newStatus}】`}</span>
              {
                resolutionChanged && !removeResolution && (
                  <>
                    <span className="c7n-Log-operation">，更新</span>
                    <span className="c7n-Log-value">【解决状态】</span>
                  </>
                )
              }
              {
                resolutionChanged && removeResolution && (
                  <>
                    <span className="c7n-Log-operation">，移除</span>
                    <span className="c7n-Log-value">【解决状态】</span>
                  </>
                )
              }
            </span>
          </span>
        );
      },
    },
  ],
  ['Estimated Start Time', {
    name: '预计开始时间',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【预计开始时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【无】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${formatDate(newString)}】`}</span>
          </span>
        );
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【预计开始时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${formatDate(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【无】</span>
          </span>
        );
      },
    },
    update: {
      transform: transformDate,
    },
  }],
  ['Estimated End Time', {
    name: '预计结束时间',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【预计结束时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【无】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${formatDate(newString)}】`}</span>
          </span>
        );
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【预计结束时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${formatDate(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【无】</span>
          </span>
        );
      },
    },
    update: {
      transform: transformDate,
    },
  }],
  ['Feature Sprint Link', {
    name: '冲刺',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【冲刺】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【无】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${newString}】`}</span>
          </span>
        );
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【冲刺】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${oldString}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【无】</span>
          </span>
        );
      },
    },
  }],
  ['Project Move', {
    name: '所属项目',
  }],
  ['Static File', {
    name: 'UI&UX文件',
    create: {
      operation: '上传',
      transform: ({ newString }: { newString: string }) => newString.split('@')[1],
    },
    delete: {
      operation: '删除',
      transform: ({ oldString }: { oldString: string }) => oldString.split('@')[1],
    },
  }],
  ['Static File Rel', {
    name: 'UI&UX文件',
    create: {
      operation: '关联',
      transform: ({ newString }: { newString: string }) => newString.split('@')[1],
    },
    delete: {
      operation: '移除',
      transform: ({ oldString }: { oldString: string }) => oldString.split('@')[1],
    },
  }],
  ['Tag', {
    name: 'Tag',
  }],
  ['environment', {
    name: '环境',
  }],
  ['mainResponsible', {
    name: '主要负责人',
  }],
  ['Actual Start Time', {
    name: '实际开始时间',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【实际开始时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【无】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${formatDate(newString)}】`}</span>
          </span>
        );
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【实际开始时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${formatDate(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【无】</span>
          </span>
        );
      },
    },
    update: {
      transform: transformDate,
    },
  }],
  ['Actual End Time', {
    name: '实际结束时间',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【实际结束时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【无】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${formatDate(newString)}】`}</span>
          </span>
        );
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【实际结束时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${formatDate(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【无】</span>
          </span>
        );
      },
    },
    update: {
      transform: transformDate,
    },
  }],
  ['participant', {
    name: '参与人',
  }],
  ['Estimate Time', {
    name: '原始预估时间',
    create: {
      render: (log: ILog) => {
        const { newString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【原始预估时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">【未预估】</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">{`【${Number(newString)}】`}</span>
          </span>
        );
      },
    },
    update: {
      transform: ({ oldString, newString }: { oldString?: string, newString?: string }) => {
        if (oldString) {
          return Number(oldString);
        } if (newString) {
          return Number(newString);
        }
        return '';
      },
    },
    delete: {
      render: (log: ILog) => {
        const { oldString } = log;
        return (
          <span>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">【原始预估时间】</span>
            <span className="c7n-Log-operation">由</span>
            <span className="c7n-Log-value">{`【${Number(oldString)}】`}</span>
            <span className="c7n-Log-operation">改变为</span>
            <span className="c7n-Log-value">【未预估】</span>
          </span>
        );
      },
    },
  }],
  ['progress', {
    name: '进度',
  }],
]);

export default fieldsMap;
