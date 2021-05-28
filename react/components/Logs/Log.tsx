import React from 'react';
import { ILog } from '@/common/types';
import { IFieldMap } from './index';

interface LogProps {
  log: ILog,
  fieldsMap: IFieldMap,
}
const Log: React.FC<LogProps> = ({ log, fieldsMap }) => {
  const getFieldConfig = () => {
    const { field, fieldName } = log;
    if (fieldsMap.get(field)) {
      return fieldsMap.get(field);
    }
    return (
      {
        name: fieldName,
      }
    );
  };

  const renderCreateDefault = () => {
    const {
      newString, oldString,
    } = log;
    const fieldConfig = getFieldConfig();
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

  const renderUpdateDefault = () => {
    const {
      oldString, newString,
    } = log;
    const fieldConfig = getFieldConfig();
    return fieldConfig && (
    <>
      {
        !(fieldConfig.update && fieldConfig.update.hidden) ? (
          <>
            <span className="c7n-Log-operation">将</span>
            <span className="c7n-Log-field">{`【${fieldConfig.name}】`}</span>
            <span className="c7n-Log-operation">由</span>
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

  const renderDeleteDefault = () => {
    const {
      oldString, newString,
    } = log;
    const fieldConfig = getFieldConfig();
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

  const fieldConfig = getFieldConfig();
  const {
    oldValue, newValue, oldString, newString,
  } = log;

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
      } if (!updateDontJudge && (updateCondition?.({ newString, oldString }) || isUpdate)) { // 修改
        return (fieldConfig.update?.render || renderUpdateDefault)(log);
      } if (!deleteDontJudge && (deleteCondition?.({ newString, oldString }) || isDelete)) { // 删除
        return (fieldConfig.delete?.render || renderDeleteDefault)(log);
      }
      return fieldConfig?.customRender ? fieldConfig?.customRender(log) : '';
    };

    return (
      <>
        {renderLog()}
      </>
    );
  }
  return <></>;
};

export default Log;
