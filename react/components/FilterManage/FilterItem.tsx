import React, {
  useState, useCallback, useRef, MutableRefObject,
} from 'react';
import { Choerodon, Action } from '@choerodon/boot';
import {
  Modal, Icon, Tooltip, TextField,
} from 'choerodon-ui/pro';
import { Tag } from '@choerodon/components';
import ObserverTextField from 'choerodon-ui/pro/lib/text-field/TextField';
import { useLockFn } from 'ahooks';
import { personalFilterApi } from '@/api';
import { IPersonalFilter } from '@/components/quick-search';
import IssueStore from '@/stores/project/issue/IssueStore';

interface Props {
  data: IPersonalFilter
  onSubmit: () => void
  onDelete: () => void
}
const FilterItem: React.FC<Props> = ({ data, onSubmit, onDelete }) => {
  const {
    filterId, name, objectVersionNumber, default: isDefault,
  } = data;
  const [isEditing, setIsEditing] = useState(false);
  const valueRef = useRef<string>(name);
  const inputRef = useRef() as MutableRefObject<ObserverTextField>;
  const handleCancel = useCallback(() => {
    setIsEditing(false);
  }, []);
  const handleSave = useCallback(async () => {
    const newValue = valueRef.current;
    if (await inputRef.current?.validate(newValue) !== true) {
      return;
    }
    IssueStore.setLoading(true);
    const updateData = {
      objectVersionNumber,
      name: newValue,
    };
    personalFilterApi.update(filterId, updateData).then(() => {
      onSubmit();
      Choerodon.prompt('修改成功');
    }).catch(() => {
      IssueStore.setLoading(false);
      Choerodon.prompt('修改失败');
    });
  }, [filterId, objectVersionNumber, onSubmit]);
  const handleDelete = useCallback(() => {
    Modal.open({
      title: '删除筛选',
      children: (
        <div>
          <p style={{ marginBottom: 10 }}>
            {`确认要删除筛选“${name}”？`}
          </p>
        </div>
      ),
      onOk() {
        IssueStore.setLoading(true);
        personalFilterApi.delete(filterId)
          .then(() => {
            onDelete();
            Choerodon.prompt('删除成功');
          }).catch(() => {
            IssueStore.setLoading(false);
            Choerodon.prompt('删除失败');
          });
      },
      onCancel() { },
      okText: '删除',
      okType: 'danger',
    });
  }, [filterId, name, onDelete]);

  const handleSetDefault = useLockFn(async () => {
    const updateData = {
      objectVersionNumber,
      name,
      default: !isDefault,
    };
    try {
      await personalFilterApi.update(filterId, updateData);
      await onSubmit();
      Choerodon.prompt('修改成功');
    } catch (error) {
      IssueStore.setLoading(false);
      Choerodon.prompt('修改失败');
    }
  });
  const checkName = useCallback(async (value: string) => {
    if (name === value) {
      return true;
    }
    const res = await personalFilterApi.checkName(value);
    if (res) {
      return '名称重复';
    }
    return true;
  }, [name]);
  return (
    <li className="c7n-filterList-item">
      {
        isEditing ? (
          <TextField
            ref={inputRef}
            required
            validator={checkName}
            autoFocus
            defaultValue={name}
            onChange={(newValue) => {
              valueRef.current = newValue;
            }}
            onInput={(e) => {
              // @ts-ignore
              inputRef.current.validate(e.target.value);
            }}
            onBlur={() => {
              setTimeout(handleCancel, 200);
            }}
            maxLength={10}
            valueChangeAction={'input' as any}
          />
        ) : (<span>{name}</span>)
      }
      {isDefault && (
        <Tag style={{ marginLeft: 'auto', marginRight: 10 }} color="green" type="border">
          默认
        </Tag>
      )}
      <span className="c7n-filterAction">
        {isEditing && (
          <>
            <Tooltip title="保存">
              <Icon
                type="check"
                onClick={handleSave}
              />
            </Tooltip>
            <Tooltip title="取消">
              <Icon
                type="close"
                onClick={handleCancel}
              />
            </Tooltip>
          </>
        )}
      </span>
      {!isEditing && (
        <Action data={[
          {
            text: isDefault ? '取消默认' : '设为默认',
            action: handleSetDefault,
          },
          {
            text: '修改',
            action: () => {
              setIsEditing(true);
            },
          },
          {
            text: '删除',
            action: handleDelete,
          },
        ]}
        />
      )}

    </li>
  );
};
export default FilterItem;
