import React, {
  useState, useCallback, useRef, MutableRefObject,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import {
  Modal, Icon, Tooltip, TextField,
} from 'choerodon-ui/pro';
import { personalFilterApi } from '@/api';
import { IPersonalFilter } from '@/components/quick-search';
import ObserverTextField from 'choerodon-ui/pro/lib/text-field/TextField';
import IssueStore from '@/stores/project/issue/IssueStore';

interface Props {
  data: IPersonalFilter
  onSubmit: () => void
  onDelete: () => void
}
const FilterItem: React.FC<Props> = ({ data, onSubmit, onDelete }) => {
  const { filterId, name, objectVersionNumber } = data;
  const [isEditing, setIsEditing] = useState(false);
  const valueRef = useRef<string>(name);
  const inputRef = useRef() as MutableRefObject<ObserverTextField>;
  const handleCancel = useCallback(() => {
    setIsEditing(false);
  }, []);
  const handleSave = useCallback(async () => {
    const newValue = valueRef.current;
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
    Modal.confirm({
      title: '删除筛选',
      children: (
        <div>
          <p style={{ marginBottom: 10 }}>
            {'确认要删除筛选 '}
            {name}
            {' ？'}
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
              // setTimeout(handleCancel, 200);
            }}
            maxLength={10}
          />
        ) : (<span>{name}</span>)
      }
      <span className="c7n-filterAction">
        {isEditing ? (
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
        ) : (
          <>
            <Tooltip title="修改">
              <Icon
                type="mode_edit"
                onClick={() => {
                  setIsEditing(true);
                }}
              />
            </Tooltip>
            <Tooltip title="删除">
              <Icon
                type="delete_forever"
                onClick={handleDelete}
              />
            </Tooltip>
          </>
        )}

      </span>
    </li>
  );
};
export default FilterItem;
