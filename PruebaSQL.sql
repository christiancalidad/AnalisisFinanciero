/* Muestre el nombre de cada cliente y el nombre de cada producto que ha comprado.*/

SELECT DISTINCT CLIENTE.Nombre as Cliente,PRODUCTO.Nombre as Producto
FROM (CLIENTE INNER JOIN PEDIDO ON CLIENTE.Cedula = PEDIDO.Cedula)
INNER JOIN PRODUCTO ON PEDIDO.Codigo_Producto = PRODUCTO.Codigo


/*¿Cuáles son los productos que no han sido vendidos?*/

SELECT PRODUCTO.Nombre as Producto FROM PRODUCTO
WHERE PRODUCTO.Codigo NOT IN
(SELECT DISTINCT Codigo_Producto FROM PEDIDO)


/* Muestre el nombre del último o los últimos productos que se hayan vendido. */


SELECT PRODUCTO.Nombre as Producto, PEDIDO.Fecha as Fecha
FROM PRODUCTO INNER JOIN PEDIDO ON PEDIDO.Codigo_Producto = PRODUCTO.Codigo
WHERE PEDIDO.Fecha = (SELECT MAX(PEDIDO.Fecha) FROM PEDIDO)


/*Muestre el nombre del cliente y el número total de productos que ha comprado ordenados por el
total de productos de manera descendente.*/

SELECT CLIENTE.Nombre as Cliente, COUNT(DISTINCT PEDIDO.Codigo_Producto) as Cantidad
FROM (CLIENTE INNER JOIN PEDIDO ON CLIENTE.Cedula = PEDIDO.Cod_Cliente)
GROUP BY CLIENTE.Nombre
ORDER BY Cantidad DESC


/* Muestre cada uno de los días de la tabla pedido y cuanto se vendió en ese día organizado de menor a
mayor fecha.*/

SELECT PEDIDO.Fecha as Fecha, PEDIDO.Cantidad*PRODUCTO.Precio as Ventas
FROM PEDIDO INNER JOIN PRODUCTO ON PEDIDO.Codigo_Producto = PRODUCTO.Codigo
GROUP BY PEDIDO.Fecha
ORDER BY PEDIDO.Fecha ASC

/*Muestre todos los datos de las personas que son de ‘Cali’ y han hecho compras superiores a 1 millón
de pesos.*/

SELECT * FROM
(SELECT PEDIDO.Cod_Cliente, PEDIDO.Cantidad*PRODUCTO.Precio as Ventas
FROM PEDIDO INNER JOIN PRODUCTO ON PEDIDO.Codigo_Producto = PRODUCTO.Codigo
GROUP BY PEDIDO.Cod_Cliente) as t INNER JOIN CLIENTE ON CLIENTE.Cedula = t.Cod_Cliente
WHERE CLIENTE.Ciudad = 'Cali' AND t.Ventas>1000000

/* Muestre todos los datos de los productos que son más baratos que un ‘Portátil’ y más caros que un
‘Lapicero’.*/
SELECT * FROM PRODUCTO
WHERE PRODUCTO.Precio>(SELECT PRODUCTO.Precio FROM PRODUCTO WHERE
PRODUCTO.Nombre = 'Lapicero')
AND PRODUCTO.Precio<(SELECT PRODUCTO.Precio FROM PRODUCTO WHERE PRODUCTO.Nombre
= ' Portátil ')

/*Muestre todos los datos de los clientes que no han comprado ningún producto.*/

SELECT * FROM CLIENTE
WHERE CLIENTE.Cedula NOT IN
(SELECT DISTINCT PEDIDO.Cod_Cliente FROM PEDIDO)
