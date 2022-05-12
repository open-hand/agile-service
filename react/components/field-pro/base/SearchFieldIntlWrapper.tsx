import React from 'react';
import useFormatMessage from '@/hooks/useFormatMessage';

type SearchFieldIntlWrapperProps<T extends { [key: string]: any } = {}> = React.PropsWithChildren<{
    children: React.ReactElement, field: any
} & T>
type SearchFieldIntlWrapper<T> = React.FC<{
    children: React.ReactElement, field: any
} & T>;
function SearchFieldIntlWrapper<T extends { [key: string]: any } = {}>({ children, field, ...otherProps }: SearchFieldIntlWrapperProps<T>) {
  const formatMessage = useFormatMessage();
  const name = field.nameKey ? formatMessage({ id: field.nameKey, defaultMessage: field.name }) || field.name : field.name;
  return React.cloneElement(children, { label: name, placeholder: name, ...otherProps });
}
export default SearchFieldIntlWrapper;
