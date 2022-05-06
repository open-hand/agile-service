import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import HeadTag from '@/components/tag/head-tag';
import styles from './index.less';

export interface IProductItem {
  name: string,
  imageUrl?: string
}

interface ProductTagsProps {
  data: IProductItem[]
  maxTagCount?: number
  className?: string
}
const ProductTags: React.FC<ProductTagsProps> = ({ data, maxTagCount = 3, className = '' }) => {
  if (!data || !data.length) {
    return null;
  }
  const visibleData = data.slice(0, maxTagCount);
  const hiddenData = data.slice(maxTagCount);
  return (
    <div className={`${styles.productTags} ${className}`}>
      {visibleData.map((product: IProductItem, index: number) => (
        <HeadTag
          name={product?.name?.slice(0, 1)?.toUpperCase()}
          className={data.length === 1 ? undefined : styles.compact}
          avatarClassName={styles.compact_tag}
          size={22}
          src={product?.imageUrl}
          tooltip={product?.name}
          text={product?.name}
          showText={data.length === 1}
          textClassName={styles.text}
        />
      ))}
      {
        hiddenData.length > 0 && (
          <Tooltip
          // @ts-ignore
            popupCls={styles.tooltip}
            title={hiddenData.map((product: IProductItem) => (
              <div className={styles.moreProduct}>
                <HeadTag
                  name={product?.name?.slice(0, 1)?.toUpperCase()}
                  src={product?.imageUrl}
                  size={20}
                  avatarClassName={styles.tag}
                  text={product?.name}
                  showText
                  tooltip={false}
                />
              </div>
            ))}
            theme="light"
          >
            <HeadTag
              className={styles.compact}
              name={`+${hiddenData.length}`}
              size={22}
              avatarClassName={styles.compact_tag}
            />
          </Tooltip>
        )
      }
    </div>
  );
};
export default ProductTags;
