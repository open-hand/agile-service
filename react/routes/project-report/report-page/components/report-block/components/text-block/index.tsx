import React, { useCallback, useEffect, useRef } from 'react';
import WYSIWYGViewer from '@/components/CKEditorViewer';
import { IReportTextBlock } from '@/routes/project-report/report-page/store';
import { useTaskContext } from '@/routes/project-report/report-preview/taskContext';

interface Props {
  data: IReportTextBlock
}
const TextBlock: React.FC<Props> = ({ data: { content, key } }) => {
  const ref = useRef<HTMLDivElement>(null);
  const { register, finish } = useTaskContext();
  register(`text-${key}`);
  const onFinish = useCallback(() => {
    finish(`text-${key}`);
  }, [finish, key]);
  useEffect(() => {
    const imgList: NodeListOf<HTMLImageElement> | undefined = ref.current?.querySelectorAll('img');
    if (imgList && imgList.length > 0) {
      let count = 0;
      imgList.forEach((img) => {
        // eslint-disable-next-line no-param-reassign
        img.onload = () => {
          count += 1;
          if (count === imgList.length) {
            onFinish();
          }
        };
        // eslint-disable-next-line no-param-reassign
        img.onerror = () => {
          count += 1;
          if (count === imgList.length) {
            onFinish();
          }
        };
      });
    } else {
      onFinish();
    }
  }, [onFinish]);
  return (
    <div style={{ padding: '10px 26px' }} ref={ref}>
      <WYSIWYGViewer value={content} />
    </div>
  );
};

export default TextBlock;
