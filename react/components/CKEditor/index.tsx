import React, {
  useRef, useMemo, useCallback,
} from 'react';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import '@choerodon/ckeditor/build/translations/zh-cn';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import { delta2Html } from '@/utils/richText';
import DecoupledEditor from '@choerodon/ckeditor';
import UploadAdapterPlugin from './plugins/UploadAdapterPlugin';
import './index.less';

const prefixCls = 'c7n-ckeditor';
function isJSON(string: string) {
  let isDelta = true;
  try {
    JSON.parse(string);
  } catch (error) {
    isDelta = false;
  }
  return isDelta;
}

interface EditorProps {
  autoFocus?: boolean
  toolbar?: boolean
  footer?: boolean
  style?: React.CSSProperties
  value?: string
  defaultValue?: string
  onChange?: (value: string) => void
  onOk?: (value: string) => void
  okText?: string
  onCancel?: () => void
  placeholder?: string
  disabled?: boolean
  height?: number
}
const Editor: React.FC<EditorProps> = ({
  value: propsValue,
  defaultValue,
  onChange,
  onOk,
  okText,
  onCancel,
  autoFocus,
  toolbar = true,
  footer,
  style,
  height,
  placeholder,
  disabled,
}) => {
  const value = useMemo(() => propsValue ?? defaultValue, [defaultValue, propsValue]);
  const data = useMemo(() => (value && isJSON(value) ? delta2Html(value) : value), [value]);
  const editorRef = useRef<any>(null);
  const handleReady = useCallback((editor) => {
    if (toolbar) {
      // Insert the toolbar before the editable area.
      editor.ui.getEditableElement().parentElement.insertBefore(
        editor.ui.view.toolbar.element,
        editor.ui.getEditableElement(),
      );
    }
    editorRef.current = editor;
    if (autoFocus) {
      editor.focus();
    }
  }, [autoFocus, toolbar]);
  const handleChange = useCallback((event, editor) => {
    const newValue = editor.getData();
    onChange && onChange(newValue);
  }, [onChange]);
  const handleSave = useCallback(async () => {
    if (onOk) {
      await onOk(editorRef.current?.getData() || '');
    }
  }, [onOk]);
  const handleCancel = useCallback(() => {
    onCancel && onCancel();
  }, [onCancel]);
  const editorConfiguration = useMemo(() => ({
    language: 'zh-cn',
    extraPlugins: [UploadAdapterPlugin],
    placeholder: placeholder ?? '描述',
  }), [placeholder]);
  return (
    <div className={prefixCls} style={style}>
      <CKEditor
        disabled={disabled}
        editor={DecoupledEditor}
        data={data ?? ''}
        onReady={handleReady}
        onChange={handleChange}
        config={editorConfiguration}
      />
      {footer && (
        <div className={`${prefixCls}-footer`}>
          <Button onClick={handleCancel} color={'blue' as ButtonColor}>
            取消
          </Button>
          <Button onClick={handleSave} color={'blue' as ButtonColor}>
            {okText ?? '确定'}
          </Button>
        </div>
      )}
    </div>
  );
};

export default Editor;
